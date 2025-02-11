{-# LANGUAGE MultiWayIf, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies #-}
module Gargantext.Utils.Jobs.Monad (
  -- * Types and classes
    JobEnv(..)
  , NumRunners
  , JobError(..)

  , MonadJob(..)

  -- * Tracking jobs status
  , MonadJobStatus(..)

  -- * Functions
  , newJobEnv
  , defaultJobSettings
  , genSecret
  , getJobsSettings
  , getJobsState
  , getJobsMap
  , getJobsQueue
  , queueJob
  , findJob
  , checkJID
  , withJob
  , handleIDError
  , removeJob
  ) where

import Gargantext.Utils.Jobs.Settings
import Gargantext.Utils.Jobs.Map
import Gargantext.Utils.Jobs.Queue
import Gargantext.Utils.Jobs.State

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Time.Clock
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import Prelude

import qualified Servant.Job.Core as SJ
import qualified Servant.Job.Types as SJ
import Data.Proxy

data JobEnv t w a = JobEnv
  { jeSettings :: JobSettings
  , jeState    :: JobsState t w a
  , jeManager  :: Manager
  }

newJobEnv
  :: (EnumBounded t, Monoid w)
  => JobSettings
  -> Map t Prio
  -> Manager
  -> IO (JobEnv t w a)
newJobEnv js prios mgr = JobEnv js <$> newJobsState js prios <*> pure mgr

type NumRunners = Int

defaultJobSettings :: NumRunners -> SJ.SecretKey -> JobSettings
defaultJobSettings numRunners k = JobSettings
  { jsNumRunners = numRunners
  , jsJobTimeout = 30 * 60 -- 30 minutes
  , jsIDTimeout  = 30 * 60 -- 30 minutes
  , jsGcPeriod   =  1 * 60 -- 1 minute
  , jsSecretKey  = k
  , jsDebugLogs  = False
  }

genSecret :: IO SJ.SecretKey
genSecret = SJ.generateSecretKey

class MonadIO m => MonadJob m t w a | m -> t w a where
  getJobEnv :: m (JobEnv t w a)

instance MonadIO m => MonadJob (ReaderT (JobEnv t w a) m) t w a where
  getJobEnv = ask

getJobsSettings :: MonadJob m t w a => m JobSettings
getJobsSettings = jeSettings <$> getJobEnv

getJobsState :: MonadJob m t w a => m (JobsState t w a)
getJobsState = jeState <$> getJobEnv

getJobsMap :: MonadJob m t w a => m (JobMap (SJ.JobID 'SJ.Safe) w a)
getJobsMap = jobsData <$> getJobsState

getJobsQueue :: MonadJob m t w a => m (Queue t (SJ.JobID 'SJ.Safe))
getJobsQueue = jobsQ <$> getJobsState

queueJob
  :: (MonadJob m t w a, Ord t)
  => t
  -> i
  -> (SJ.JobID 'SJ.Safe -> i -> Logger w -> IO a)
  -> m (SJ.JobID 'SJ.Safe)
queueJob jobkind input f = do
  js <- getJobsSettings
  st <- getJobsState
  liftIO (pushJob jobkind input f js st)

findJob
  :: MonadJob m t w a
  => SJ.JobID 'SJ.Safe
  -> m (Maybe (JobEntry (SJ.JobID 'SJ.Safe) w a))
findJob jid = do
  jmap <- getJobsMap
  liftIO $ lookupJob jid jmap

data JobError
  =
  -- | We expected to find a job tagged internall as \"job\", but we found the input @T.Text@ instead.
    InvalidIDType T.Text
  -- | The given ID expired.
  | IDExpired Int
  | InvalidMacID T.Text
  | UnknownJob Int
  | JobException SomeException
  deriving Show

checkJID
  :: MonadJob m t w a
  => SJ.JobID 'SJ.Unsafe
  -> m (Either JobError (SJ.JobID 'SJ.Safe))
checkJID (SJ.PrivateID tn n t d) = do
  now <- liftIO getCurrentTime
  js <- getJobsSettings
  if | tn /= "job" -> pure (Left $ InvalidIDType $ T.pack tn)
     | now > addUTCTime (fromIntegral $ jsIDTimeout js) t -> pure (Left $ IDExpired n)
     | d /= SJ.macID tn (jsSecretKey js) t n -> pure (Left $ InvalidMacID $ T.pack d)
     | otherwise -> pure $ Right (SJ.PrivateID tn n t d)

withJob
  :: MonadJob m t w a
  => SJ.JobID 'SJ.Unsafe
  -> (SJ.JobID 'SJ.Safe -> JobEntry (SJ.JobID 'SJ.Safe) w a -> m r)
  -> m (Either JobError (Maybe r))
withJob jid f = do
  r <- checkJID jid
  case r of
    Left e -> pure (Left e)
    Right jid' -> do
      mj <- findJob jid'
      case mj of
        Nothing -> pure (Right Nothing)
        Just j  -> Right . Just <$> f jid' j

handleIDError
  :: MonadError e m
  => (JobError -> e)
  -> m (Either JobError a)
  -> m a
handleIDError toE act = act >>= \r -> case r of
  Left err -> throwError (toE err)
  Right a  -> pure a

removeJob
  :: (Ord t, MonadJob m t w a)
  => Bool -- is it queued (and we have to remove jid from queue)
  -> t
  -> SJ.JobID 'SJ.Safe
  -> m ()
removeJob queued t jid = do
  q <- getJobsQueue
  m <- getJobsMap
  liftIO . atomically $ do
    when queued $
      deleteQueue t jid q
    deleteJob jid m

--
-- Tracking jobs status
--

-- | A monad to query for the status of a particular job /and/ submit updates for in-progress jobs.
class MonadJobStatus m where

  -- | This is type family for the concrete 'JobHandle' that is associated to
  -- a job when it starts and it can be used to query for its completion status. Different environment
  -- can decide how this will look like.
  type JobHandle      m :: Type

  type JobType        m :: Type
  type JobOutputType  m :: Type
  type JobEventType   m :: Type

  -- | A job handle that doesn't do anything. Sometimes useful in all those circumstances
  -- where we need to test a function taking a 'JobHandle' as input but we are not interested
  -- in the progress tracking.
  noJobHandle :: Proxy m -> JobHandle m

  -- | Retrevies the latest 'JobEventType' from the underlying monad. It can be
  -- used to query the latest status for a particular job, given its 'JobHandle' as input.
  getLatestJobStatus :: JobHandle m -> m (JobEventType m)

  -- | Adds an extra \"tracer\" that logs events to the passed action. Produces
  -- a new 'JobHandle'.
  withTracer :: Logger (JobEventType m) -> JobHandle m -> (JobHandle m -> m a) -> m a

  -- Creating events

  -- | Start tracking a new 'JobEventType' with 'n' remaining steps.
  markStarted :: Int -> JobHandle m -> m ()

  -- | Mark 'n' steps of the job as succeeded, while simultaneously substracting this number
  -- from the remaining steps.
  markProgress :: Int -> JobHandle m -> m ()

  -- | Mark 'n' step of the job as failed, while simultaneously substracting this number
  -- from the remaining steps. Attach an optional error message to the failure.
  markFailure :: Int -> Maybe T.Text -> JobHandle m -> m ()

  -- | Finish tracking a job by marking all the remaining steps as succeeded.
  markComplete :: JobHandle m -> m ()

  -- | Finish tracking a job by marking all the remaining steps as failed. Attach an optional
  -- message to the failure.
  markFailed :: Maybe T.Text -> JobHandle m -> m ()

  -- | Add 'n' more steps to the running computation, they will be marked as remaining.
  addMoreSteps :: MonadJobStatus m => Int -> JobHandle m -> m ()
