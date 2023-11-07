{-|
Module      : Test.Database.Types
Description : GarganText tests
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Test.Database.Types where

import Control.Exception
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.IORef
import Data.Map qualified as Map
import Data.Pool
import Data.Text qualified as T
import Database.PostgreSQL.Simple qualified as PG
import Database.Postgres.Temp qualified as Tmp
import Gargantext hiding (to)
import Gargantext.API.Admin.EnvTypes
import Gargantext.API.Admin.EnvTypes qualified as EnvTypes
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Prelude
import Gargantext.Core.Mail.Types (HasMail(..))
import Gargantext.Core.NLP (HasNLPServer(..))
import Gargantext.Core.NodeStory
import Gargantext.Database.Prelude (HasConfig(..), HasConnectionPool(..))
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Prelude.Config
import Gargantext.Prelude.Mail.Types (MailConfig(..), LoginType(NoAuth))
import Gargantext.System.Logging (HasLogger(..), Logger, MonadLogger(..))
import Gargantext.Utils.Jobs
import Network.URI (parseURI)
import Prelude qualified
import System.Log.FastLogger qualified as FL

newtype Counter = Counter { _Counter :: IORef Int }
  deriving Eq

instance Prelude.Show Counter where
  show (Counter _) = "Counter"

emptyCounter :: IO Counter
emptyCounter = Counter <$> newIORef 0

nextCounter :: Counter -> IO Int
nextCounter (Counter ref) = atomicModifyIORef' ref (\old -> (succ old, old))

data TestEnv = TestEnv {
    test_db                  :: !DBHandle
  , test_config              :: !GargConfig
  , test_nodeStory           :: !NodeStoryEnv
  , test_usernameGen         :: !Counter
  , test_logger              :: !(Logger (GargM TestEnv GargError))
  }

newtype TestMonad a = TestMonad { runTestMonad :: ReaderT TestEnv IO a }
  deriving ( Functor, Applicative, Monad
           , MonadReader TestEnv, MonadError IOException
           , MonadBase IO
           , MonadBaseControl IO
           , MonadFail
           , MonadIO
           )

instance MonadJobStatus TestMonad where
  type JobHandle      TestMonad = EnvTypes.ConcreteJobHandle GargError
  type JobType        TestMonad = GargJob
  type JobOutputType  TestMonad = JobLog
  type JobEventType   TestMonad = JobLog

  getLatestJobStatus _  = TestMonad (pure noJobLog)
  withTracer _ jh n     = n jh
  markStarted _ _       = TestMonad $ pure ()
  markProgress _ _      = TestMonad $ pure ()
  markFailure _ _ _     = TestMonad $ pure ()
  markComplete _        = TestMonad $ pure ()
  markFailed _ _        = TestMonad $ pure ()
  addMoreSteps _ _      = TestMonad $ pure ()

data DBHandle = DBHandle {
    _DBHandle :: Pool PG.Connection
  , _DBTmp    :: Tmp.DB
  }

instance HasNodeError IOException where
  _NodeError = prism' (Prelude.userError . show) (const Nothing)

instance HasConnectionPool TestEnv where
  connPool = to (_DBHandle . test_db)

instance HasConfig TestEnv where
  hasConfig = to test_config

instance HasMail TestEnv where
  mailSettings = to $ const (MailConfig { _mc_mail_host       = "localhost"
                                        , _mc_mail_port       = 25
                                        , _mc_mail_user       = "test"
                                        , _mc_mail_from       = "test@localhost"
                                        , _mc_mail_password   = "test"
                                        , _mc_mail_login_type = NoAuth })

instance HasNodeStoryEnv TestEnv where
  hasNodeStory = to test_nodeStory


instance HasNodeStoryVar TestEnv where
  hasNodeStoryVar = hasNodeStory . nse_getter

instance HasNodeStoryImmediateSaver TestEnv where
  hasNodeStoryImmediateSaver = hasNodeStory . nse_saver_immediate

instance HasNodeArchiveStoryImmediateSaver TestEnv where
  hasNodeArchiveStoryImmediateSaver = hasNodeStory . nse_archive_saver_immediate


coreNLPConfig :: NLPServerConfig
coreNLPConfig =
  let uri = parseURI "http://localhost:9000"
  in NLPServerConfig CoreNLP (fromMaybe (Prelude.error "parseURI for nlpServerConfig failed") uri)


instance HasNLPServer TestEnv where
  nlpServer = to $ const (Map.singleton EN coreNLPConfig)

instance MonadLogger (GargM TestEnv GargError) where
  getLogger = asks test_logger

instance HasLogger (GargM TestEnv GargError) where
  data instance Logger (GargM TestEnv GargError) =
    GargTestLogger {
      test_logger_mode :: Mode
    , test_logger_set  :: FL.LoggerSet
    }
  type instance LogInitParams (GargM TestEnv GargError) = Mode
  type instance LogPayload (GargM TestEnv GargError)    = FL.LogStr
  initLogger                = \mode -> do
    test_logger_set <- liftIO $ FL.newStderrLoggerSet FL.defaultBufSize
    pure $ GargTestLogger mode test_logger_set
  destroyLogger             = \GargTestLogger{..}  -> liftIO $ FL.rmLoggerSet test_logger_set
  logMsg = \(GargTestLogger mode logger_set) lvl msg -> do
    let pfx = "[" <> show lvl <> "] " :: Text
    when (lvl `elem` (modeToLoggingLevels mode)) $
      liftIO $ FL.pushLogStrLn logger_set $ FL.toLogStr pfx <> msg
  logTxt lgr lvl msg = logMsg lgr lvl (FL.toLogStr $ T.unpack msg)
