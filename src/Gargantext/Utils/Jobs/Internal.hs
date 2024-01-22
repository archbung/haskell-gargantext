{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Gargantext.Utils.Jobs.Internal (
    serveJobsAPI
  -- * Internals for testing
  , newJob
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.Aeson (ToJSON)
import Data.Foldable (toList)
import Data.Monoid
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Prelude
import Servant.API.Alternative

import Gargantext.Utils.Jobs.Map
import Gargantext.Utils.Jobs.Monad

import qualified Data.Text as T
import qualified Servant.Client as C
import qualified Servant.Job.Async as SJ
import qualified Servant.Job.Client as SJ
import qualified Servant.Job.Core as SJ
import qualified Servant.Job.Types as SJ

serveJobsAPI
  :: ( Ord t, Exception e, MonadError e m
     , MonadJob m t (Seq event) output
     , ToJSON e, ToJSON event, ToJSON output
     , Foldable callback
     )
  => (SJ.JobID 'SJ.Safe -> LoggerM m event -> JobHandle m)
  -> m env
  -> t
  -> (JobError -> e)
  -> (env -> JobHandle m -> input -> IO (Either e output))
  -> SJ.AsyncJobsServerT' ctI ctO callback event input output m
serveJobsAPI newJobHandle getenv t joberr f
     = newJob newJobHandle getenv t f (SJ.JobInput undefined Nothing)
  :<|> newJob newJobHandle getenv t f
  :<|> serveJobAPI t joberr

serveJobAPI
  :: forall (m :: Type -> Type) e t event output.
     (Ord t, MonadError e m, MonadJob m t (Seq event) output)
  => t
  -> (JobError -> e)
  -> SJ.JobID 'SJ.Unsafe
  -> SJ.AsyncJobServerT event output m
serveJobAPI t joberr jid' = wrap' (killJob t)
                       :<|> wrap' pollJob
                       :<|> wrap (waitJob joberr)

  where wrap
          :: forall a.
             (SJ.JobID 'SJ.Safe -> JobEntry (SJ.JobID 'SJ.Safe) (Seq event) output -> m a)
          -> m a
        wrap g = do
          jid <- handleIDError joberr (checkJID jid')
          job <- maybe (throwError $ joberr $ UnknownJob (SJ._id_number jid)) pure =<< findJob jid
          g jid job

        wrap' g limit offset = wrap (g limit offset)

newJob
  :: ( Ord t, Exception e, MonadJob m t (Seq event) output
     , ToJSON e, ToJSON event, ToJSON output
     , Foldable callbacks
     )
  => (SJ.JobID 'SJ.Safe -> LoggerM m event -> JobHandle m)
  -> m env
  -> t
  -> (env -> JobHandle m -> input -> IO (Either e output))
  -> SJ.JobInput callbacks input
  -> m (SJ.JobStatus 'SJ.Safe event)
newJob newJobHandle getenv jobkind f input = do
  je <- getJobEnv
  env <- getenv
  let postCallback m = forM_ (input ^. SJ.job_callback) $ \url ->
        C.runClientM (SJ.clientMCallback m)
                     (C.mkClientEnv (jeManager je) (url  ^. SJ.base_url))

      pushLog logF = \w -> do
        postCallback (SJ.mkChanEvent w)
        logF w

      f' jId inp logF = do
        r <- f env (newJobHandle jId (liftIO . pushLog logF . Seq.singleton)) inp
        case r of
          Left e  -> postCallback (SJ.mkChanError e) >> throwIO e
          Right a -> postCallback (SJ.mkChanResult a) >> pure a

  jid <- queueJob jobkind (input ^. SJ.job_input) f'
  pure (SJ.JobStatus jid [] SJ.IsPending Nothing)

pollJob
  :: MonadJob m t (Seq event) output
  => Maybe SJ.Limit
  -> Maybe SJ.Offset
  -> SJ.JobID 'SJ.Safe
  -> JobEntry (SJ.JobID 'SJ.Safe) (Seq event) output
  -> m (SJ.JobStatus 'SJ.Safe event)
pollJob limit offset jid je = do
  (logs, status, merr) <- case jTask je of
    QueuedJ _    -> pure (mempty, SJ.IsPending, Nothing)
    RunningJ rj  -> (,,) <$> liftIO (rjGetLog rj)
                         <*> pure SJ.IsRunning
                         <*> pure Nothing
    DoneJ ls r ->
      let st = either (const SJ.IsFailure) (const SJ.IsFinished) r
          me = either (Just . T.pack . show) (const Nothing) r
      in pure (ls, st, me)
  -- /NOTE/: We need to be careful with the ordering of the logs here:
  -- we want to return the logs ordered from the newest to the oldest,
  -- because the API will use 'limit' to show only the newest ones,
  -- taking 'limit' of them from the front of the list.
  --
  -- Due to the fact we do not force any 'Ord' constraint on an 'event' type,
  -- and it would be inefficient to reverse the list here, it's important
  -- that the concrete implementation of 'rjGetLog' returns the logs in the
  -- correct order.
  pure $ SJ.jobStatus jid limit offset (toList logs) status merr

waitJob
  :: (MonadError e m, MonadJob m t (Seq event) output)
  => (JobError -> e)
  -> SJ.JobID 'SJ.Safe
  -> JobEntry (SJ.JobID 'SJ.Safe) (Seq event) output
  -> m (SJ.JobOutput output)
waitJob joberr jid je = do
  r <- case jTask je of
    QueuedJ _qj -> do
      m <- getJobsMap
      erj <- waitTilRunning
      case erj of
        Left res -> pure res
        Right rj -> do
          (res, _logs) <- liftIO (waitJobDone jid rj m)
          pure res
    RunningJ rj -> do
      m <- getJobsMap
      (res, _logs) <- liftIO (waitJobDone jid rj m)
      pure res
    DoneJ _ls res -> pure res
  either (throwError . joberr . JobException) (pure . SJ.JobOutput) r

  where waitTilRunning =
          findJob jid >>= \mjob -> case mjob of
            Nothing -> error "impossible"
            Just je' -> case jTask je' of
              QueuedJ _qj -> do
                liftIO $ threadDelay 50000 -- wait 50ms
                waitTilRunning
              RunningJ rj -> pure (Right rj)
              DoneJ _ls res -> pure (Left res)

killJob
  :: (Ord t, MonadJob m t (Seq event) output)
  => t
  -> Maybe SJ.Limit
  -> Maybe SJ.Offset
  -> SJ.JobID 'SJ.Safe
  -> JobEntry (SJ.JobID 'SJ.Safe) (Seq event) output
  -> m (SJ.JobStatus 'SJ.Safe event)
killJob t limit offset jid je = do
  (logs, status, merr) <- case jTask je of
    QueuedJ _ -> do
      removeJob True t jid
      pure (mempty, SJ.IsKilled, Nothing)
    RunningJ rj -> do
      liftIO $ cancel (rjAsync rj)
      lgs <- liftIO (rjGetLog rj)
      removeJob False t jid
      pure (lgs, SJ.IsKilled, Nothing)
    DoneJ lgs r -> do
      let st = either (const SJ.IsFailure) (const SJ.IsFinished) r
          me = either (Just . T.pack . show) (const Nothing) r
      removeJob False t jid
      pure (lgs, st, me)
  -- /NOTE/: Same proviso as in 'pollJob' applies here.
  pure $ SJ.jobStatus jid limit offset (toList logs) status merr
