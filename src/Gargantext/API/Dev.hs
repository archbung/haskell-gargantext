{-|
Module      : Gargantext.API.Dev
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

-- Use only for dev/repl
module Gargantext.API.Dev where

import Control.Lens (view)
import Control.Monad (fail)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple qualified as PGS
import Gargantext.API.Admin.EnvTypes ( DevEnv(..), Mode(Dev) )
import Gargantext.API.Admin.Settings ( devJwkFile, devSettings, newPool )
import Gargantext.API.Errors.Types ( BackendInternalError )
import Gargantext.API.Prelude ( GargM )
import Gargantext.Core.NLP (nlpServerMap)
import Gargantext.Core.NodeStory (fromDBNodeStoryEnv)
import Gargantext.Database.Prelude (Cmd', Cmd'', connPool, databaseParameters, runCmd)
import Gargantext.Prelude
import Gargantext.Prelude.Config (readConfig)
import Gargantext.Prelude.Mail qualified as Mail
import Gargantext.Prelude.NLP qualified as NLP
import Gargantext.System.Logging ( withLoggerHoisted )
import Servant ( ServerError )

type IniPath  = FilePath
-------------------------------------------------------------------
withDevEnv :: IniPath -> (DevEnv -> IO a) -> IO a
withDevEnv iniPath k = withLoggerHoisted Dev $ \logger -> do
  env <- newDevEnv logger
  k env -- `finally` cleanEnv env

  where
    newDevEnv logger = do
      cfg     <- readConfig         iniPath
      dbParam <- databaseParameters iniPath
      --nodeStory_env <- fromDBNodeStoryEnv (_gc_repofilepath cfg)
      pool    <- newPool            dbParam
      nodeStory_env <- fromDBNodeStoryEnv pool
      setts   <- devSettings devJwkFile
      mail    <- Mail.readConfig iniPath
      nlp_config <- NLP.readConfig iniPath
      pure $ DevEnv
        { _dev_env_pool     = pool
        , _dev_env_logger     = logger
        , _dev_env_nodeStory  = nodeStory_env
        , _dev_env_settings = setts
        , _dev_env_config   = cfg
        , _dev_env_mail     = mail
        , _dev_env_nlp      = nlpServerMap nlp_config
        }

-- | Run Cmd Sugar for the Repl (GHCI)
runCmdRepl :: Show err => Cmd'' DevEnv err a -> IO a
runCmdRepl f = withDevEnv "gargantext.ini" $ \env -> runCmdDev env f

runCmdReplServantErr :: Cmd'' DevEnv ServerError a -> IO a
runCmdReplServantErr = runCmdRepl

-- In particular this writes the repo file after running
-- the command.
-- This function is constrained to the DevEnv rather than
-- using HasConnectionPool and HasRepoVar.
runCmdDev :: Show err => DevEnv -> Cmd'' DevEnv err a -> IO a
runCmdDev env f =
  either (fail . show) pure =<< runCmd env f

runCmdGargDev :: DevEnv -> GargM DevEnv BackendInternalError a -> IO a
runCmdGargDev env cmd =
  either (fail . show) pure =<< runExceptT (runReaderT cmd env)

runCmdDevNoErr :: DevEnv -> Cmd' DevEnv () a -> IO a
runCmdDevNoErr = runCmdDev

runCmdDevServantErr :: DevEnv -> Cmd' DevEnv ServerError a -> IO a
runCmdDevServantErr = runCmdDev

runCmdReplEasy :: Cmd'' DevEnv BackendInternalError a -> IO a
runCmdReplEasy f = withDevEnv "gargantext.ini" $ \env -> runCmdDev env f

-- | Execute a function that takes PSQL.Connection from the DB pool as
--   first parameter.
--   e.g.: runCmdReplEasyDB $ \c -> getNodeStory' c
runCmdReplEasyDB :: (PGS.Connection -> IO a) -> IO a
runCmdReplEasyDB f = runCmdReplEasy $ view connPool >>= (\p -> liftBase $ withResource p f)
