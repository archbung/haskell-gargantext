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
import Data.Pool
import Gargantext
import Gargantext.API.Admin.EnvTypes
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Prelude
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Prelude.Config
import Gargantext.Utils.Jobs
import Prelude
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Postgres.Temp as Tmp
import qualified Gargantext.API.Admin.EnvTypes as EnvTypes

newtype Counter = Counter { _Counter :: IORef Int }
  deriving Eq

instance Show Counter where
  show (Counter _) = "Counter"

emptyCounter :: IO Counter
emptyCounter = Counter <$> newIORef 0

nextCounter :: Counter -> IO Int
nextCounter (Counter ref) = atomicModifyIORef' ref (\old -> (succ old, old))

data TestEnv = TestEnv {
    test_db                  :: !DBHandle
  , test_config              :: !GargConfig
  , test_usernameGen         :: !Counter
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
  _NodeError = prism' (userError . show) (const Nothing)

instance HasConnectionPool TestEnv where
  connPool = to (_DBHandle . test_db)

instance HasConfig TestEnv where
  hasConfig = to test_config

