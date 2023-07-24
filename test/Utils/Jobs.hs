{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE NumericUnderscores  #-}
module Utils.Jobs (test) where

import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import Data.Either
import Data.List
import Data.Sequence (Seq, (|>), fromList)
import Data.Time
import Debug.RecoverRTTI (anythingToString)
import Prelude
import System.IO.Unsafe
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Client (Manager)
import Test.Hspec
import Test.Hspec.Expectations.Contrib (annotate)
import qualified Servant.Job.Types as SJ
import qualified Servant.Job.Core  as SJ

import Gargantext.Utils.Jobs.Internal (newJob)
import Gargantext.Utils.Jobs.Map
import Gargantext.Utils.Jobs.Monad hiding (withJob)
import Gargantext.Utils.Jobs.Queue (applyPrios, defaultPrios)
import Gargantext.Utils.Jobs.State
import Gargantext.API.Prelude
import Gargantext.API.Admin.EnvTypes as EnvTypes
import Gargantext.API.Admin.Orchestrator.Types
import Control.Concurrent.Async


data JobT = A
          | B
          | C
          | D
          deriving (Eq, Ord, Show, Enum, Bounded)

-- | This type models the schedule picked up by the orchestrator.
newtype JobSchedule = JobSchedule { _JobSchedule :: Seq JobT } deriving (Eq, Show)

addJobToSchedule :: JobT -> MVar JobSchedule -> IO ()
addJobToSchedule jobt mvar = do
  modifyMVar_ mvar $ \js -> do
    let js' = js { _JobSchedule = _JobSchedule js |> jobt }
    pure js'

data Counts = Counts { countAs :: Int, countBs :: Int }
  deriving (Eq, Show)

jobDuration :: Int
jobDuration = 100000

type Timer = TVar Bool

-- | Use in conjuction with 'registerDelay' to create an 'STM' transaction
-- that will simulate the duration of a job by waiting the timeout registered
-- by 'registerDelay' before continuing.
waitTimerSTM :: Timer -> STM ()
waitTimerSTM tv = do
  v <- readTVar tv
  check v

-- | Samples the running jobs from the first 'TVar' and write them
-- in the queue.
sampleRunningJobs :: Timer -> TVar [String] -> TQueue [String]-> STM ()
sampleRunningJobs timer runningJs samples = do
  waitTimerSTM timer
  runningNow <- readTVar runningJs
  case runningNow of
    [] -> pure () -- ignore empty runs, when the system is kickstarting.
    xs -> writeTQueue samples xs

-- | The aim of this test is to ensure that the \"max runners\" setting is
-- respected, i.e. we have no more than \"N\" jobs running at the same time.
testMaxRunners :: IO ()
testMaxRunners = do
  -- max runners = 2 with default settings
  let num_jobs = 4
  k <- genSecret
  let settings = defaultJobSettings 2 k
  st :: JobsState JobT [String] () <- newJobsState settings defaultPrios
  now <- getCurrentTime
  runningJs   <- newTVarIO []
  samples     <- newTQueueIO
  remainingJs <- newTVarIO num_jobs

  -- Not the most elegant solution, but in order to test the \"max runners\"
  -- parameter we start an asynchronous computation that continuously reads the content
  -- of 'runningJs' and at the end ensures that this value was
  -- always <= \"max_runners" (but crucially not 0).

  asyncReader <- async $ forever $ do 
    samplingFrequency <- registerDelay 100_000
    atomically $ sampleRunningJobs samplingFrequency runningJs samples
  
  let duration = 1_000_000
      j num _jHandle _inp _l = do
        durationTimer <- registerDelay duration
        -- NOTE: We do the modification of the 'runningJs' and the rest
        -- in two transactions on purpose, to give a chance to the async
        -- sampler to sample the status of the world.
        atomically $  modifyTVar runningJs (\xs -> ("Job #" ++ show num) : xs)
        atomically $ do
          waitTimerSTM durationTimer
          modifyTVar runningJs (\xs -> filter (/=("Job #" ++ show num)) xs)
          modifyTVar remainingJs pred
      jobs = [ (A, j n) | n <- [1..num_jobs::Int] ]

  atomically $ forM_ jobs $ \(t, f) -> void $
    pushJobWithTime now t () f settings st

  let waitFinished = atomically $ do
        x <- readTVar remainingJs
        check (x == 0)

  -- Wait for the jobs to finish, then stop the sampler.
  waitFinished
  cancel asyncReader

  -- Check that we got /some/ samples and for each of them,
  -- let's check only two runners at max were alive.
  allSamples <- atomically $ flushTQueue samples
  length allSamples `shouldSatisfy` (> 0)

  forM_ allSamples $ \runLog -> do
    annotate "predicate to satisfy: (x `isInfixOf` [\"Job #1\", \"Job #2\"] || x `isInfixOf` [\"Job #3\", \"Job #4\"]" $
      shouldSatisfy (sort runLog)
                    (\x -> x `isInfixOf` ["Job #1", "Job #2"] || x `isInfixOf` ["Job #3", "Job #4"])

testPrios :: IO ()
testPrios = do
  k <- genSecret
  -- Use a single runner, so that we can check the order of execution
  -- without worrying about the runners competing with each other.
  let settings = defaultJobSettings 1 k
      prios    = [(B, 10), (C, 1), (D, 5)]
  st :: JobsState JobT [String] () <- newJobsState settings $
    applyPrios prios defaultPrios -- B has the highest priority
  pickedSchedule <- newMVar (JobSchedule mempty)
  let j jobt _jHandle _inp _l = addJobToSchedule jobt pickedSchedule
      jobs = [ (A, j A)
             , (C, j C)
             , (B, j B)
             , (D, j D)
             ]

  -- Push all the jobs in the same STM transaction, so that they are all stored in the queue by
  -- the time 'popQueue' gets called.
  now <- getCurrentTime
  atomically $ forM_ jobs $ \(t, f) -> void $ pushJobWithTime now t () f settings st

  -- wait for the jobs to finish, waiting for more than the total duration,
  -- so that we are sure that all jobs have finished, then check the schedule.
  threadDelay jobDuration
  finalSchedule <- readMVar pickedSchedule
  finalSchedule `shouldBe` JobSchedule (fromList [B, D, C, A])

testExceptions :: IO ()
testExceptions = do
  k <- genSecret
  let settings = defaultJobSettings 1 k
  st :: JobsState JobT [String] () <- newJobsState settings defaultPrios
  jid <- pushJob A ()
    (\_jHandle _inp _log -> readFile "/doesntexist.txt" >>= putStrLn)
    settings st
  -- Wait 1 second to make sure the job is finished.
  threadDelay $ 1_000_000
  mjob <- lookupJob jid (jobsData st)
  case mjob of
    Nothing ->  fail "lookupJob failed, job not found!"
    Just je ->  case jTask je of
      DoneJ _ r   -> isLeft r `shouldBe` True
      unexpected  -> fail $ "Expected job to be done, but got: " <> anythingToString unexpected
  return ()

testFairness :: IO ()
testFairness = do
  k <- genSecret
  let settings = defaultJobSettings 1 k
  st :: JobsState JobT [String] () <- newJobsState settings defaultPrios
  pickedSchedule <- newMVar (JobSchedule mempty)
  let j jobt _jHandle _inp _l = addJobToSchedule jobt pickedSchedule
      jobs = [ (A, j A)
             , (A, j A)
             , (B, j B)
             , (A, j A)
             , (A, j A)
             ]
  time <- getCurrentTime
  -- in this scenario we simulate two types of jobs all with
  -- all the same level of priority: our queue implementation
  -- will behave as a classic FIFO, keeping into account the
  -- time of arrival.
  atomically $ forM_ (zip [0,2 ..] jobs) $ \(timeDelta, (t, f)) -> void $
    pushJobWithTime (addUTCTime (fromInteger timeDelta) time) t () f settings st

  threadDelay jobDuration
  finalSchedule <- readMVar pickedSchedule
  finalSchedule `shouldBe` JobSchedule (fromList [A, A, B, A, A])


newtype MyDummyMonad a =
  MyDummyMonad { _MyDummyMonad :: GargM Env GargError a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

instance MonadJob MyDummyMonad GargJob (Seq JobLog) JobLog where
  getJobEnv = MyDummyMonad getJobEnv

instance MonadJobStatus MyDummyMonad where
  type JobHandle      MyDummyMonad = EnvTypes.ConcreteJobHandle GargError
  type JobType        MyDummyMonad = GargJob
  type JobOutputType  MyDummyMonad = JobLog
  type JobEventType   MyDummyMonad = JobLog

  getLatestJobStatus jId      = MyDummyMonad (getLatestJobStatus jId)
  withTracer _ jh n           = n jh
  markStarted n jh            = MyDummyMonad (markStarted n jh)
  markProgress steps jh       = MyDummyMonad (markProgress steps jh)
  markFailure steps mb_msg jh = MyDummyMonad (markFailure steps mb_msg jh)
  markComplete jh             = MyDummyMonad (markComplete jh)
  markFailed mb_msg jh        = MyDummyMonad (markFailed mb_msg jh)

runMyDummyMonad :: Env -> MyDummyMonad a -> IO a
runMyDummyMonad env m = do
  res <- runExceptT . flip runReaderT env $ _MyDummyMonad m
  case res of
    Left e -> throwIO e
    Right x -> pure x

testTlsManager :: Manager
testTlsManager = unsafePerformIO newTlsManager
{-# NOINLINE testTlsManager #-}

withJob :: Env
        -> (JobHandle MyDummyMonad -> () -> MyDummyMonad ())
        -> IO (SJ.JobStatus 'SJ.Safe JobLog)
withJob env f = runMyDummyMonad env $ MyDummyMonad $
  -- the job type doesn't matter in our tests, we use a random one, as long as it's of type 'GargJob'.
  newJob @_ @GargError mkJobHandle (pure env) RecomputeGraphJob (\_ hdl input ->
    runMyDummyMonad env $ (Right <$> (f hdl input >> getLatestJobStatus hdl))) (SJ.JobInput () Nothing)

withJob_ :: Env
         -> (JobHandle MyDummyMonad -> () -> MyDummyMonad ())
         -> IO ()
withJob_ env f = void (withJob env f)

newTestEnv :: IO Env
newTestEnv = do
  k <- genSecret
  let settings = defaultJobSettings 1 k
  myEnv <- newJobEnv settings defaultPrios testTlsManager
  pure $ Env
       { _env_settings  = error "env_settings not needed, but forced somewhere (check StrictData)"
       , _env_logger    = error "env_logger not needed, but forced somewhere (check StrictData)"
       , _env_pool      = error "env_pool not needed, but forced somewhere (check StrictData)"
       , _env_nodeStory = error "env_nodeStory not needed, but forced somewhere (check StrictData)"
       , _env_manager   = testTlsManager
       , _env_self_url  = error "self_url not needed, but forced somewhere (check StrictData)"
       , _env_scrapers  = error "scrapers not needed, but forced somewhere (check StrictData)"
       , _env_jobs      = myEnv
       , _env_config    = error "config not needed, but forced somewhere (check StrictData)"
       , _env_mail      = error "mail not needed, but forced somewhere (check StrictData)"
       , _env_nlp       = error "nlp not needed, but forced somewhere (check StrictData)"
       }

testFetchJobStatus :: IO ()
testFetchJobStatus = do
  myEnv <- newTestEnv
  evts <- newMVar []

  withJob_ myEnv $ \hdl _input -> do
    mb_status <- getLatestJobStatus hdl

    -- now let's log something
    markStarted 10 hdl
    mb_status' <- getLatestJobStatus hdl
    markProgress 5 hdl
    mb_status'' <- getLatestJobStatus hdl

    liftIO $ modifyMVar_ evts (\xs -> pure $ mb_status : mb_status' : mb_status'' : xs)
    pure ()

  threadDelay 500_000
  -- Check the events
  readMVar evts >>= \expected -> map _scst_remaining expected `shouldBe` [Nothing, Just 10, Just 5]

testFetchJobStatusNoContention :: IO ()
testFetchJobStatusNoContention = do
  myEnv <- newTestEnv

  evts1 <- newMVar []
  evts2 <- newMVar []

  let job1 = \() -> withJob_ myEnv $ \hdl _input -> do
        markStarted 100 hdl
        mb_status <- getLatestJobStatus hdl
        liftIO $ modifyMVar_ evts1 (\xs -> pure $ mb_status : xs)
        pure ()

  let job2 = \() -> withJob_ myEnv $ \hdl _input -> do
        markStarted 50 hdl
        mb_status <- getLatestJobStatus hdl
        liftIO $ modifyMVar_ evts2 (\xs -> pure $ mb_status : xs)
        pure ()

  Async.forConcurrently_ [job1, job2] ($ ())
  threadDelay 500_000
  -- Check the events
  readMVar evts1 >>= \expected -> map _scst_remaining expected `shouldBe` [Just 100]
  readMVar evts2 >>= \expected -> map _scst_remaining expected `shouldBe` [Just 50]

testMarkProgress :: IO ()
testMarkProgress = do
  myEnv <- newTestEnv
  evts  <- newTBQueueIO 7
  let getStatus hdl = do
        liftIO $ threadDelay 100_000
        st <- getLatestJobStatus hdl
        liftIO $ atomically $ writeTBQueue evts st
      readAllEvents = do
        allEventsArrived <- isFullTBQueue evts
        if allEventsArrived then flushTBQueue evts else retry

  withJob_ myEnv $ \hdl _input -> do
    markStarted 10 hdl
    getStatus hdl

    markProgress 1 hdl
    getStatus hdl

    markFailure 1 Nothing hdl
    getStatus hdl

    markFailure 1 (Just "boom") hdl

    getStatus hdl
    markComplete hdl

    getStatus hdl
    markStarted 5 hdl
    markProgress 1 hdl

    getStatus hdl
    markFailed (Just "kaboom") hdl

    getStatus hdl

  [jl0, jl1, jl2, jl3, jl4, jl5, jl6] <- atomically readAllEvents

  -- Check the events are what we expect
  jl0 `shouldBe` JobLog { _scst_succeeded = Just 0
                        , _scst_failed    = Just 0
                        , _scst_remaining = Just 10
                        , _scst_events    = Just []
                        }
  jl1 `shouldBe` JobLog { _scst_succeeded = Just 1
                        , _scst_failed    = Just 0
                        , _scst_remaining = Just 9
                        , _scst_events    = Just []
                        }
  jl2 `shouldBe` JobLog { _scst_succeeded = Just 1
                        , _scst_failed    = Just 1
                        , _scst_remaining = Just 8
                        , _scst_events    = Just []
                        }
  jl3 `shouldBe` JobLog { _scst_succeeded = Just 1
                        , _scst_failed    = Just 2
                        , _scst_remaining = Just 7
                        , _scst_events    = Just [
                            ScraperEvent { _scev_message = Just "boom"
                                         , _scev_level = Just "ERROR"
                                         , _scev_date = Nothing }
                        ]
                        }
  jl4 `shouldBe` JobLog { _scst_succeeded = Just 8
                        , _scst_failed    = Just 2
                        , _scst_remaining = Just 0
                        , _scst_events    = Just [
                            ScraperEvent { _scev_message = Just "boom"
                                         , _scev_level = Just "ERROR"
                                         , _scev_date = Nothing }
                        ]
                        }
  jl5 `shouldBe` JobLog { _scst_succeeded = Just 1
                        , _scst_failed    = Just 0
                        , _scst_remaining = Just 4
                        , _scst_events    = Just []
                        }
  jl6 `shouldBe` JobLog { _scst_succeeded = Just 1
                        , _scst_failed    = Just 4
                        , _scst_remaining = Just 0
                        , _scst_events    = Just [
                            ScraperEvent { _scev_message = Just "kaboom"
                                         , _scev_level = Just "ERROR"
                                         , _scev_date = Nothing }
                        ]
                        }

test :: Spec
test = do
  describe "job queue" $ do
    it "respects max runners limit" $
      testMaxRunners
    it "respects priorities" $
      testPrios
    it "can handle exceptions" $
      testExceptions
    it "fairly picks equal-priority-but-different-kind jobs" $
      testFairness
  describe "job status update and tracking" $ do
    it "can fetch the latest job status" $
      testFetchJobStatus
    it "can spin two separate jobs and track their status separately" $
      testFetchJobStatusNoContention
    it "marking stuff behaves as expected" $
      testMarkProgress
