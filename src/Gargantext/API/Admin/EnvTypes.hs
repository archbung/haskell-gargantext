-- |

{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase #-}

module Gargantext.API.Admin.EnvTypes (
    GargJob(..)
  , Env(..)
  , Mode(..)
  , modeToLoggingLevels
  , mkJobHandle
  , env_logger
  , env_manager
  , env_self_url
  , menv_firewall
  , dev_env_logger

  , MockEnv(..)
  , DevEnv(..)
  , DevJobHandle(..)
  , ConcreteJobHandle -- opaque
  ) where

import Control.Lens hiding (Level, (:<))
import Control.Monad.Except
import Control.Monad.Reader
import Data.List ((\\))
import Data.Pool (Pool)
import Data.Sequence (ViewL(..), viewl)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Admin.Types
import Gargantext.API.Job
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Core.Mail.Types (HasMail, mailSettings)
import Gargantext.Core.NLP (NLPServerMap, HasNLPServer(..))
import Gargantext.Core.NodeStory
import Gargantext.Database.Prelude (HasConnectionPool(..), HasConfig(..))
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig(..))
import Gargantext.Prelude.Mail.Types (MailConfig)
import Gargantext.System.Logging
import Gargantext.Utils.Jobs.Map (LoggerM, J(..), jTask, rjGetLog)
import Gargantext.Utils.Jobs.Monad qualified as Jobs
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl)
import Servant.Job.Async (HasJobEnv(..), Job)
import Servant.Job.Async qualified as SJ
import Servant.Job.Core qualified
import System.Log.FastLogger qualified as FL

data Mode = Dev | Mock | Prod
  deriving (Show, Read, Generic)

-- | Given the 'Mode' the server is running in, it returns the list of
-- allowed levels. For example for production we ignore everything which
-- has priority lower than "warning".
modeToLoggingLevels :: Mode -> [LogLevel]
modeToLoggingLevels = \case
   Dev  -> [minBound .. maxBound]
   Mock -> [minBound .. maxBound]
   -- For production, accepts everything but DEBUG.
   Prod -> [minBound .. maxBound] \\ [DEBUG]

instance MonadLogger (GargM Env GargError) where
  getLogger = asks _env_logger

instance HasLogger (GargM Env GargError) where
  data instance Logger (GargM Env GargError)  =
    GargLogger {
        logger_mode    :: Mode
      , logger_set     :: FL.LoggerSet
      }
  type instance LogInitParams (GargM Env GargError) = Mode
  type instance LogPayload (GargM Env GargError)    = FL.LogStr
  initLogger                = \mode -> do
    logger_set <- liftIO $ FL.newStderrLoggerSet FL.defaultBufSize
    pure $ GargLogger mode logger_set
  destroyLogger             = \GargLogger{..}  -> liftIO $ FL.rmLoggerSet logger_set
  logMsg = \(GargLogger mode logger_set) lvl msg -> do
    let pfx = "[" <> show lvl <> "] " :: Text
    when (lvl `elem` (modeToLoggingLevels mode)) $
      liftIO $ FL.pushLogStrLn logger_set $ FL.toLogStr pfx <> msg
  logTxt lgr lvl msg = logMsg lgr lvl (FL.toLogStr $ T.unpack msg)


data GargJob
  = TableNgramsJob
  | ForgotPasswordJob
  | UpdateNgramsListJobJSON
  | UpdateNgramsListJobCSV
  | AddContactJob
  | AddFileJob
  | DocumentFromWriteNodeJob
  | UpdateNodeJob
  | UploadFrameCalcJob
  | UploadDocumentJob
  | NewNodeJob
  | AddCorpusQueryJob
  | AddCorpusFormJob
  | AddCorpusFileJob
  | AddAnnuaireFormJob
  | RecomputeGraphJob
  deriving (Show, Eq, Ord, Enum, Bounded)

-- Do /not/ treat the data types of this type as strict, because it's convenient
-- to be able to partially initialise things like an 'Env' during tests, without
-- having to specify /everything/. This means that when we /construct/ an 'Env',
-- we need to remember to force the fields to WHNF at that point.
data Env = Env
  { _env_settings  :: ~Settings
  , _env_logger    :: ~(Logger (GargM Env GargError))
  , _env_pool      :: ~(Pool Connection)
  , _env_nodeStory :: ~NodeStoryEnv
  , _env_manager   :: ~Manager
  , _env_self_url  :: ~BaseUrl
  , _env_scrapers  :: ~ScrapersEnv
  , _env_jobs      :: ~(Jobs.JobEnv GargJob (Seq JobLog) JobLog)
  , _env_config    :: ~GargConfig
  , _env_mail      :: ~MailConfig
  , _env_nlp       :: ~NLPServerMap
  }
  deriving (Generic)

makeLenses ''Env

instance HasConfig Env where
  hasConfig = env_config

instance HasConnectionPool Env where
  connPool = env_pool

instance HasNodeStoryEnv Env where
  hasNodeStory = env_nodeStory

instance HasNodeStoryVar Env where
  hasNodeStoryVar = hasNodeStory . nse_getter

instance HasNodeStoryImmediateSaver Env where
  hasNodeStoryImmediateSaver = hasNodeStory . nse_saver_immediate

instance HasNodeArchiveStoryImmediateSaver Env where
  hasNodeArchiveStoryImmediateSaver = hasNodeStory . nse_archive_saver_immediate

instance HasSettings Env where
  settings = env_settings

instance HasMail Env where
  mailSettings = env_mail

instance HasNLPServer Env where
  nlpServer = env_nlp

instance Servant.Job.Core.HasEnv Env (Job JobLog JobLog) where
  _env = env_scrapers . Servant.Job.Core._env

instance HasJobEnv Env JobLog JobLog where
  job_env = env_scrapers

instance Jobs.MonadJob (GargM Env err) GargJob (Seq JobLog) JobLog where
  getJobEnv = asks (view env_jobs)

-- | The /concrete/ 'JobHandle' in use with our 'GargM' (production) monad. Its
-- constructor it's not exported, to not leak internal details of its implementation.
data ConcreteJobHandle err = JobHandle {
      _jh_id     :: !(SJ.JobID 'SJ.Safe)
    , _jh_logger :: LoggerM (GargM Env err) JobLog
    }

-- | Creates a new /concrete/ 'JobHandle', given its underlying 'JobID' and the logging function to
-- be used to report the status.
mkJobHandle :: SJ.JobID 'SJ.Safe
            -> LoggerM (GargM Env err) JobLog
            -> ConcreteJobHandle err
mkJobHandle jId = JobHandle jId

-- | Updates the status of a 'JobHandle' by using the input 'updateJobStatus' function.
updateJobProgress :: ConcreteJobHandle err -> (JobLog -> JobLog) -> GargM Env err ()
updateJobProgress hdl@(JobHandle _ logStatus) updateJobStatus =
  Jobs.getLatestJobStatus hdl >>= logStatus . updateJobStatus

instance Jobs.MonadJobStatus (GargM Env err) where

  type JobHandle      (GargM Env err) = ConcreteJobHandle err
  type JobType        (GargM Env err) = GargJob
  type JobOutputType  (GargM Env err) = JobLog
  type JobEventType   (GargM Env err) = JobLog

  getLatestJobStatus (JobHandle jId _) = do
    mb_jb <- Jobs.findJob jId
    case mb_jb of
      Nothing -> pure noJobLog
      Just j  -> case jTask j of
        QueuedJ _   -> pure noJobLog
        RunningJ rj -> liftIO (rjGetLog rj) <&>
                         \lgs -> case viewl lgs of
                                   EmptyL -> noJobLog
                                   l :< _ -> l
        DoneJ lgs _ -> pure $ case viewl lgs of
                                   EmptyL -> noJobLog
                                   l :< _ -> l

  withTracer extraLogger (JobHandle jId logger) n = n (JobHandle jId (\w -> logger w >> liftIO (extraLogger w)))

  markStarted n jh = updateJobProgress jh (const $ jobLogStart (RemainingSteps n))

  markProgress steps jh = updateJobProgress jh (jobLogProgress steps)

  markFailure steps mb_msg jh =
    updateJobProgress jh (\latest -> case mb_msg of
                                       Nothing  -> jobLogFailures steps latest
                                       Just msg -> addErrorEvent msg (jobLogFailures steps latest)
                         )

  markComplete jh = updateJobProgress jh jobLogComplete

  markFailed mb_msg jh =
    updateJobProgress jh (\latest -> case mb_msg of
                                       Nothing  -> jobLogFailTotal latest
                                       Just msg -> jobLogFailTotalWithMessage msg latest
                         )

  addMoreSteps steps jh = updateJobProgress jh (jobLogAddMore steps)

data MockEnv = MockEnv
  { _menv_firewall :: !FireWall
  }
  deriving (Generic)

makeLenses ''MockEnv

instance MonadLogger (GargM DevEnv GargError) where
  getLogger = asks _dev_env_logger

instance HasLogger (GargM DevEnv GargError) where
  data instance Logger (GargM DevEnv GargError)  =
    GargDevLogger {
        dev_logger_mode    :: Mode
      , dev_logger_set     :: FL.LoggerSet
      }
  type instance LogInitParams (GargM DevEnv GargError) = Mode
  type instance LogPayload (GargM DevEnv GargError)    = FL.LogStr
  initLogger                = \mode -> do
    dev_logger_set <- liftIO $ FL.newStderrLoggerSet FL.defaultBufSize
    pure $ GargDevLogger mode dev_logger_set
  destroyLogger             = \GargDevLogger{..}  -> liftIO $ FL.rmLoggerSet dev_logger_set
  logMsg = \(GargDevLogger mode logger_set) lvl msg -> do
    let pfx = "[" <> show lvl <> "] " :: Text
    when (lvl `elem` (modeToLoggingLevels mode)) $
      liftIO $ FL.pushLogStrLn logger_set $ FL.toLogStr pfx <> msg
  logTxt lgr lvl msg = logMsg lgr lvl (FL.toLogStr $ T.unpack msg)

data DevEnv = DevEnv
  { _dev_env_settings  :: !Settings
  , _dev_env_config    :: !GargConfig
  , _dev_env_logger    :: !(Logger (GargM DevEnv GargError))
  , _dev_env_pool      :: !(Pool Connection)
  , _dev_env_nodeStory :: !NodeStoryEnv
  , _dev_env_mail      :: !MailConfig
  , _dev_env_nlp       :: !NLPServerMap
  }

makeLenses ''DevEnv

-- | Our /mock/ job handle.
data DevJobHandle = DevJobHandle

instance Jobs.MonadJobStatus (GargM DevEnv err) where

  type JobHandle (GargM DevEnv err) = DevJobHandle

  type JobType        (GargM DevEnv err) = GargJob
  type JobOutputType  (GargM DevEnv err) = JobLog
  type JobEventType   (GargM DevEnv err) = JobLog

  getLatestJobStatus DevJobHandle = pure noJobLog

  withTracer _ DevJobHandle n = n DevJobHandle

  markStarted _ _ = pure ()

  markProgress _ _ = pure ()

  markFailure _ _ _ = pure ()

  markComplete _ = pure ()

  markFailed _ _ = pure ()

  addMoreSteps _ _ = pure ()

instance HasConfig DevEnv where
  hasConfig = dev_env_config

instance HasConnectionPool DevEnv where
  connPool = dev_env_pool

instance HasSettings DevEnv where
  settings = dev_env_settings


instance HasNodeStoryEnv DevEnv where
  hasNodeStory = dev_env_nodeStory

instance HasNodeStoryVar DevEnv where
  hasNodeStoryVar = hasNodeStory . nse_getter

instance HasNodeStoryImmediateSaver DevEnv where
  hasNodeStoryImmediateSaver = hasNodeStory . nse_saver_immediate

instance HasNodeArchiveStoryImmediateSaver DevEnv where
  hasNodeArchiveStoryImmediateSaver = hasNodeStory . nse_archive_saver_immediate

instance HasMail DevEnv where
  mailSettings = dev_env_mail

instance HasNLPServer DevEnv where
  nlpServer = dev_env_nlp
