{-|
Module      : Gargantext.API.Prelude
Description : Server API main Types
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds  #-}

module Gargantext.API.Prelude
  ( module Gargantext.API.Prelude
  , HasServerError(..)
  , serverError
  )
  where

import Control.Lens ((#))
import Crypto.JOSE.Error as Jose
import Data.Aeson.Types
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Admin.Types
import Gargantext.API.Errors.Class
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Core.NLP (HasNLPServer)
import Gargantext.Core.NodeStory
import Gargantext.Core.Types
import Gargantext.Database.Prelude (CmdM, CmdRandom, HasConnectionPool, HasConfig)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Query.Tree
import Gargantext.Prelude
import Gargantext.System.Logging
import Gargantext.Utils.Jobs.Monad (MonadJobStatus(..), JobHandle)
import Servant
import Servant.Job.Async
import Servant.Job.Core (HasServerError(..), serverError)

joseError :: (MonadError e m, HasJoseError e) => Jose.Error -> m a
joseError = throwError . (_JoseError #)

type HasJobEnv' env = HasJobEnv env JobLog JobLog

type EnvC env =
  ( HasConnectionPool env
  , HasSettings       env  -- TODO rename HasDbSettings
  , HasJobEnv         env JobLog JobLog
  , HasConfig         env
  , HasNodeStoryEnv   env
  , HasMail           env
  , HasNLPServer      env
  )

type ErrC err =
  ( HasNodeError       err
  , HasValidationError err
  , HasTreeError       err
  , HasServerError     err
  , HasJoseError       err
--  , ToJSON           err -- TODO this is arguable
  , Exception          err
  )

type GargServerC env err m =
  ( CmdRandom    env err m
  , HasNodeStory env err m
  , EnvC         env
  , ErrC             err
  , ToJSON           err
  )

type GargServerT env err m api = GargServerC env err m => ServerT api m

type GargServer api = forall env err m. MonadLogger m => GargServerT env err m api

-- This is the concrete monad. It needs to be used as little as possible.
type GargM env err = ReaderT env (ExceptT err IO)
-- This is the server type using GargM. It needs to be used as little as possible.
-- Instead, prefer GargServer, GargServerT.
type GargServerM env err api = (EnvC env, ErrC err) => ServerT api (GargM env err)

-------------------------------------------------------------------
-- | This Type is needed to prepare the function before the GargServer
type GargNoServer t =
  forall env err m. GargNoServer' env err m => m t

type GargNoServer' env err m =
  ( CmdM           env err m
  , HasNodeStory   env err m
  , HasSettings    env
  , HasNodeError       err
  )

------------------------------------------------------------------------
-- | Utils
-- | Simulate logs
simuLogs  :: (MonadBase IO m, MonadJobStatus m) => JobHandle m -> Int -> m ()
simuLogs jobHandle t = do
  markStarted t jobHandle
  mapM_ (const simuTask) $ take t ([0,1..] :: [Int])
  markComplete jobHandle
  where
    simuTask = do
      let m = (10 :: Int) ^ (6 :: Int)
      liftBase $ threadDelay (m*5)
      markProgress 1 jobHandle
