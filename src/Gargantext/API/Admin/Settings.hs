{-|
Module      : Gargantext.API.Admin.Settings
Description : Settings of the API (Server and Client)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TODO-SECURITY: Critical
-}



{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Gargantext.API.Admin.Settings
    where

import Codec.Serialise (Serialise(), serialise)
import Control.Lens
import Control.Monad.Logger (LogLevel(..))
import Control.Monad.Reader
import Data.ByteString.Lazy qualified as L
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Database.PostgreSQL.Simple (Connection, connect, close, ConnectInfo)
import Gargantext.API.Admin.EnvTypes
import Gargantext.API.Admin.Settings.CORS
import Gargantext.API.Admin.Types
import Gargantext.API.Errors.Types
import Gargantext.API.Prelude
import Gargantext.Core.NLP (nlpServerMap)
import Gargantext.Core.NodeStory
import Gargantext.Database.Prelude (databaseParameters, hasConfig)
import Gargantext.Prelude
import Gargantext.Prelude.Config (gc_js_job_timeout, gc_js_id_timeout)
import Gargantext.Prelude.Config ({-GargConfig(..),-} {-gc_repofilepath,-} readConfig)
import Gargantext.Prelude.Mail qualified as Mail
import Gargantext.Prelude.NLP qualified as NLP
import Gargantext.System.Logging
import Gargantext.Utils.Jobs qualified as Jobs
import Gargantext.Utils.Jobs.Monad qualified as Jobs
import Gargantext.Utils.Jobs.Queue qualified as Jobs
import Gargantext.Utils.Jobs.Settings qualified as Jobs
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Auth.Server (defaultJWTSettings, CookieSettings(..), XsrfCookieSettings(..), defaultCookieSettings, defaultXsrfCookieSettings, readKey, writeKey)
import Servant.Client (parseBaseUrl)
import Servant.Job.Async (newJobEnv, defaultSettings)
import System.Directory
import System.IO (hClose)
import System.IO.Temp (withTempFile)

devSettings :: FilePath -> IO Settings
devSettings jwkFile = do
  jwkExists <- doesFileExist jwkFile
  when (not jwkExists) $ writeKey jwkFile
  jwk       <- readKey jwkFile
  gargCorsSettings <- loadGargCorsSettings
  pure $ Settings
    { _corsSettings = gargCorsSettings
    , _appPort = 3000
    , _logLevelLimit = LevelDebug
--    , _dbServer = "localhost"
    , _sendLoginEmails = LogEmailToConsole
    , _scrapydUrl = fromMaybe (panicTrace "Invalid scrapy URL") $ parseBaseUrl "http://localhost:6800"
    , _cookieSettings = defaultCookieSettings { cookieXsrfSetting = Just xsrfCookieSetting } -- TODO-SECURITY tune
    , _jwtSettings = defaultJWTSettings jwk -- TODO-SECURITY tune
    }
  where
    xsrfCookieSetting = defaultXsrfCookieSettings { xsrfExcludeGet = True }

{- NOT USED YET
import System.Environment (lookupEnv)

reqSetting :: FromHttpApiData a => Text -> IO a
reqSetting name = do
    e <- fromMaybe (panic $ "Missing " <> name) <$> lookupEnv (unpack name)
    pure $ either (panic $ "Unable to parse " <> name) identity $ parseUrlPiece $ pack e

optSetting :: FromHttpApiData a => Text -> a -> IO a
optSetting name d = do
    me <- lookupEnv (unpack name)
    case me of
        Nothing -> pure d
        Just e -> pure $ either (panic $ "Unable to parse " <> name) identity $ parseUrlPiece $ pack e

settingsFromEnvironment :: IO Settings
settingsFromEnvironment =
    Settings <$> (encodeUtf8 <$> reqSetting "ALLOWED_ORIGIN")
             <*> (encodeUtf8 <$> reqSetting "ALLOWED_HOST")
             <*> optSetting "PORT" 3000
             <*> (parseLogLevel <$> optSetting "LOG_LEVEL" "warn")
             <*> reqSetting "DB_SERVER"
             <*> (parseJwk <$> reqSetting "JWT_SECRET")
             <*> optSetting "SEND_EMAIL" SendEmailViaAws
-}

-----------------------------------------------------------------------
-- | RepoDir FilePath configuration
type RepoDirFilePath = FilePath

repoSnapshot :: RepoDirFilePath -> FilePath
repoSnapshot repoDir = repoDir <> "/repo.cbor"



-- This assumes we own the lock on repoSnapshot.
repoSaverAction :: RepoDirFilePath -> Serialise a => a -> IO ()
repoSaverAction repoDir a = do
  withTempFile repoDir "tmp-repo.cbor" $ \fp h -> do
    -- printDebug "repoSaverAction" fp
    L.hPut h $ serialise a
    hClose h
    renameFile fp (repoSnapshot repoDir)



{-
-- The use of mkDebounce makes sure that repoSaverAction is not called too often.
-- If repoSaverAction start taking more time than the debounceFreq then it should
-- be increased.
mkRepoSaver :: RepoDirFilePath -> MVar NgramsRepo -> IO (IO ())
mkRepoSaver repoDir repo_var = mkDebounce settings'
  where
    settings' = defaultDebounceSettings
                 { debounceFreq   = let n = 6 :: Int in 10^n  -- 1 second
                 , debounceAction = withMVar repo_var (repoSaverAction repoDir)
                   -- Here this not only `readMVar` but `takeMVar`.
                   -- Namely while repoSaverAction is saving no other change
                   -- can be made to the MVar.
                   -- This might be not efficent and thus reconsidered later.
                   -- However this enables to safely perform a *final* save.
                   -- See `cleanEnv`.
                   -- Future work:
                   -- Add a new MVar just for saving.
                 }

-}
{-
readRepoEnv :: FilePath -> IO RepoEnv
readRepoEnv repoDir = do
  -- Does file exist ? :: Bool
  _repoDir <- createDirectoryIfMissing True repoDir

  repoFile <- doesFileExist (repoSnapshot repoDir)

  -- Is file not empty ? :: Bool
  repoExists <- if repoFile
             then (>0) <$> getFileSize (repoSnapshot repoDir)
             else pure False

  mlock <- tryLockFile (repoSnapshot repoDir) Exclusive
  lock <- maybe (panic "Repo file already locked") pure mlock

  mvar <- newMVar =<<
    if repoExists
      then do
        -- e_repo <- eitherDecodeStrict <$> deserialise <$> L.readFile repoSnapshot
        repo <- deserialise <$> L.readFile (repoSnapshot repoDir)
        -- repo   <- either fail pure e_repo
        let archive = (repoSnapshot repoDir) <> ".v" <> show (repo ^. r_version)
        copyFile (repoSnapshot repoDir) archive
        pure repo
      else
        pure initRepo
  -- TODO save in DB here
  saver <- mkRepoSaver repoDir mvar
  pure $ RepoEnv { _renv_var = mvar, _renv_saver = saver, _renv_lock = lock }
--}

devJwkFile :: FilePath
devJwkFile = "dev.jwk"

newEnv :: Logger (GargM Env BackendInternalError) -> PortNumber -> FilePath -> IO Env
newEnv logger port file = do
  !manager_env  <- newTlsManager
  !settings'    <- devSettings devJwkFile <&> appPort .~ port -- TODO read from 'file'
  when (port /= settings' ^. appPort) $
    panicTrace "TODO: conflicting settings of port"

  !config_env   <- readConfig file
  prios         <- withLogger () $ \ioLogger -> Jobs.readPrios ioLogger (file <> ".jobs")
  let prios' = Jobs.applyPrios prios Jobs.defaultPrios
  putStrLn ("Overrides: " <> show prios :: Text)
  putStrLn ("New priorities: " <> show prios' :: Text)
  !self_url_env  <- parseBaseUrl $ "http://0.0.0.0:" <> show port
  dbParam        <- databaseParameters file
  !pool          <- newPool dbParam
  --nodeStory_env <- fromDBNodeStoryEnv (_gc_repofilepath config_env)
  !nodeStory_env <- fromDBNodeStoryEnv pool
  !scrapers_env  <- newJobEnv defaultSettings manager_env

  secret        <- Jobs.genSecret
  let jobs_settings = (Jobs.defaultJobSettings 1 secret)
                        & Jobs.l_jsJobTimeout .~ (fromIntegral $ config_env ^. hasConfig ^. gc_js_job_timeout)
                        & Jobs.l_jsIDTimeout  .~ (fromIntegral $ config_env ^. hasConfig ^. gc_js_id_timeout)
  !jobs_env     <- Jobs.newJobEnv jobs_settings prios' manager_env
  !config_mail  <- Mail.readConfig file
  !nlp_env      <- nlpServerMap <$> NLP.readConfig file

{-  An 'Env' by default doesn't have strict fields, but when constructing one in production
  we want to force them to WHNF to avoid accumulating unnecessary thunks.
-}
  pure $ Env
    { _env_settings  = settings'
    , _env_logger    = logger
    , _env_pool      = pool
    , _env_nodeStory = nodeStory_env
    , _env_manager   = manager_env
    , _env_scrapers  = scrapers_env
    , _env_jobs      = jobs_env
    , _env_self_url  = self_url_env
    , _env_config    = config_env
    , _env_mail      = config_mail
    , _env_nlp       = nlp_env
    }

newPool :: ConnectInfo -> IO (Pool Connection)
newPool param = Pool.newPool $ Pool.setNumStripes (Just 1) $ Pool.defaultPoolConfig (connect param) close (60*60) 8

{-
cleanEnv :: (HasConfig env, HasRepo env) => env -> IO ()
cleanEnv env = do
  r <- takeMVar (env ^. repoEnv . renv_var)
  repoSaverAction (env ^. hasConfig . gc_repofilepath) r
  unlockFile (env ^. repoEnv . renv_lock)
--}
