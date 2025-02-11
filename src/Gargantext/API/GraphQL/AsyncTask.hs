{-|
Module      : Gargantext.API.GraphQL.AsyncTask
Description :
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.AsyncTask where

import Control.Lens
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Morpheus.Types ( GQLType, Resolver, QUERY )
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..))
import Gargantext.API.Errors.Types
import Gargantext.API.Prelude (GargM, HasJobEnv')
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import Gargantext.Prelude
import Servant.Job.Async (HasJobEnv(job_env), jenv_jobs, job_async)
import Servant.Job.Core (env_item, env_map, env_state_mvar)

data JobLogArgs
  = JobLogArgs
    { job_log_id :: Int
    } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env BackendInternalError)

resolveJobLogs
  :: (HasConnectionPool env, HasConfig env, HasJobEnv' env)
  => JobLogArgs -> GqlM e env (Map Int JobLog)
resolveJobLogs JobLogArgs { job_log_id } = dbJobLogs job_log_id

dbJobLogs
  :: (HasConnectionPool env, HasConfig env, HasJobEnv' env)
  => Int -> GqlM e env (Map Int JobLog)
dbJobLogs _job_log_id = do
  --getJobLogs job_log_id
  lift $ do
    env <- ask
    --val <- liftBase $ readMVar $ env ^. job_env . jenv_jobs . env_state_mvar
    var <- liftIO $ readMVar (env ^. job_env . jenv_jobs . env_state_mvar)
    let envItems = var ^. env_map
    -- printDebug "[dbJobLogs] env ^. job_env ^. jenv_jobs" $ length $ IntMap.keys envItems
    -- printDebug "[dbJobLogs] job_log_id" job_log_id
    --pure $ IntMap.elems val
    liftIO $ do
      let jobsList = IntMap.toList $ IntMap.map (\e -> e ^. env_item . job_async) envItems
      results <- mapM (\(k, v) -> do
                          p <- poll v
                          let kv = case p of
                                Nothing -> Nothing
                                Just p' -> case p' of
                                  Left _ -> Nothing
                                  Right p'' -> Just (k, p'')
                          pure kv) jobsList
      pure $ Map.fromList $ catMaybes results
