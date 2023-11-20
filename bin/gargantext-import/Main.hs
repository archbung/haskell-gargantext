{-|
Module      : Main.hs
Description : Gargantext Import Corpus
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Import a corpus binary.

 -}

{-# LANGUAGE Strict            #-}

module Main where

import Data.Either
import qualified Data.Text as Text

import Gargantext.API.Dev (withDevEnv, runCmdGargDev)
import Gargantext.API.Admin.EnvTypes (DevEnv(..), DevJobHandle(..))
import Gargantext.API.Errors.Types
import Gargantext.API.Node () -- instances
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Types.Query (Limit)
import Gargantext.Database.Action.Flow (flowCorpusFile, flowAnnuaire, TermType(..))
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Types.Node (CorpusId)
import Gargantext.Prelude
import Gargantext.Core.Text.Corpus.Parsers (FileFormat(..), FileType(..))
import Gargantext.Utils.Jobs (MonadJobStatus, JobHandle)

main :: IO ()
main = do
  [fun, user, name, iniPath, limit, corpusPath] <- getArgs

  --{-

  let
    --tt = (Unsupervised EN 6 0 Nothing)
    tt = (Multi EN)
    format = CsvGargV3 -- CsvHal --WOS
    limit' = case (readMaybe limit :: Maybe Limit) of
      Nothing -> panic $ "Cannot read limit: " <> (Text.pack limit)
      Just l  -> l
    corpus :: forall m. (FlowCmdM DevEnv BackendInternalError m, MonadJobStatus m, JobHandle m ~ DevJobHandle) => m CorpusId
    corpus = flowCorpusFile (UserName $ cs user) (Left (cs name :: Text)) limit' tt  format Plain corpusPath Nothing DevJobHandle

    corpusCsvHal :: forall m. (FlowCmdM DevEnv BackendInternalError m, MonadJobStatus m, JobHandle m ~ DevJobHandle) => m CorpusId
    corpusCsvHal = flowCorpusFile (UserName $ cs user) (Left (cs name :: Text)) limit' tt CsvHal Plain corpusPath Nothing DevJobHandle

    annuaire :: forall m. (FlowCmdM DevEnv BackendInternalError m, MonadJobStatus m, JobHandle m ~ DevJobHandle) => m CorpusId
    annuaire = flowAnnuaire (UserName $ cs user) (Left "Annuaire") (Multi EN) corpusPath DevJobHandle

  {-
  let debatCorpus :: forall m. FlowCmdM DevEnv BackendInternalError m => m CorpusId
      debatCorpus = do
        docs <- liftIO ( splitEvery 500
                       <$> take (read limit :: Int)
                       <$> readFile corpusPath
                       :: IO [[GrandDebatReference ]]
                       )
        flowCorpus (Text.pack user) (Text.pack name) (Multi FR) (map (map toHyperdataDocument) docs)
  --}

  withDevEnv iniPath $ \env -> do
    _ <- if fun == "corpus"
          then runCmdGargDev env corpus
          else pure 0 --(cs "false")

    _ <- if fun == "corpusCsvHal"
          then runCmdGargDev env corpusCsvHal
          else pure 0 --(cs "false")

    _ <- if fun == "annuaire"
            then runCmdGargDev env annuaire
            else pure 0
    {-
    _ <- if corpusType == "csv"
            then runCmdDev env csvCorpus
            else if corpusType == "debat"
              then runCmdDev env debatCorpus
              else panic "corpusType unknown: try \"csv\" or \"debat\""
    -}
    pure ()
