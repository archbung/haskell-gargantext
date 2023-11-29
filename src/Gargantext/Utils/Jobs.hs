{-|
Module      : Gargantext.Utils.Jobs
Description : Gargantext utilities
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Gargantext.Utils.Jobs (
  -- * Serving the JOBS API
    serveJobsAPI
  -- * Parsing and reading @GargJob@s from disk
  , readPrios
  -- * Handy re-exports
  , MonadJobStatus(..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (ToJSON)
import Prelude
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import qualified Data.Text as T

import Gargantext.API.Admin.EnvTypes
import Gargantext.API.Errors.Types
import Gargantext.API.Prelude
import qualified Gargantext.Utils.Jobs.Internal as Internal
import Gargantext.Utils.Jobs.Monad
import Gargantext.System.Logging

import qualified Servant.Job.Async as SJ

jobErrorToGargError
  :: JobError -> BackendInternalError
jobErrorToGargError = InternalJobError

serveJobsAPI
  :: (
     Foldable callbacks
   , Ord (JobType m)
   , Show (JobType m)
   , ToJSON (JobEventType m)
   , ToJSON (JobOutputType m)
   , MonadJobStatus m
   , m ~ (GargM Env BackendInternalError)
   , JobEventType m ~ JobOutputType m
   , MonadLogger m
   )
  => JobType m
  -> (JobHandle m -> input -> m ())
  -> SJ.AsyncJobsServerT' ctI ctO callbacks (JobEventType m) input (JobOutputType m) m
serveJobsAPI jobType f = Internal.serveJobsAPI mkJobHandle ask jobType jobErrorToGargError $ \env jHandle i -> do
  runExceptT $ flip runReaderT env $ do
    $(logLocM) INFO (T.pack $ "Running job of type: " ++ show jobType)
    f jHandle i
    getLatestJobStatus jHandle

parseGargJob :: String -> Maybe GargJob
parseGargJob s = case s of
  "tablengrams" -> Just TableNgramsJob
  "forgotpassword" -> Just ForgotPasswordJob
  "updatengramslistjson" -> Just UpdateNgramsListJobJSON
  "updatengramslistcsv" -> Just UpdateNgramsListJobCSV
  "addcontact" -> Just AddContactJob
  "addfile" -> Just AddFileJob
  "documentfromwritenode" -> Just DocumentFromWriteNodeJob
  "updatenode" -> Just UpdateNodeJob
  "updateframecalc" -> Just UploadFrameCalcJob
  "updatedocument" -> Just UploadDocumentJob
  "newnode" -> Just NewNodeJob
  "addcorpusquery" -> Just AddCorpusQueryJob
  "addcorpusform" -> Just AddCorpusFormJob
  "addcorpusfile" -> Just AddCorpusFileJob
  "addannuaireform" -> Just AddAnnuaireFormJob
  "recomputegraph" -> Just RecomputeGraphJob
  _ -> Nothing

parsePrios :: [String] -> IO [(GargJob, Int)]
parsePrios []       = pure []
parsePrios (x : xs) = (:) <$> go x <*> parsePrios xs
  where go s = case break (=='=') s of
          ([], _) -> error "parsePrios: empty jobname?"
          (prop, valS)
            | Just val <- readMaybe (tail valS)
            , Just j   <- parseGargJob prop -> pure (j, val)
            | otherwise                  -> error $
              "parsePrios: invalid input. " ++ show (prop, valS)

readPrios :: Logger IO -> FilePath -> IO [(GargJob, Int)]
readPrios logger fp = do
  exists <- doesFileExist fp
  case exists of
    False -> do
      $(logLoc) logger WARNING $ T.pack $ fp ++ " doesn't exist, using default job priorities."
      pure []
    True -> parsePrios . lines =<< readFile fp
