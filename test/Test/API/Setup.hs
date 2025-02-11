{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings    #-}

module Test.API.Setup where

-- import Gargantext.Prelude (printDebug)
import Control.Lens
import Control.Monad.Reader
import Gargantext.API (makeApp)
import Gargantext.API.Admin.EnvTypes (Mode(Mock), Env (..))
import Gargantext.API.Admin.Settings
import Gargantext.API.Admin.Types
import Gargantext.API.Errors.Types
import Gargantext.API.Prelude
import Gargantext.Core.NLP
import Gargantext.Core.NodeStory
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.Flow
import Gargantext.Database.Action.User.New
import Gargantext.Database.Admin.Config (userMaster, corpusMasterName)
import Gargantext.Database.Admin.Trigger.Init
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node (getOrMkList)
import Gargantext.Database.Query.Tree.Root (MkCorpusUser(..))
import Gargantext.Prelude.Config
import Gargantext.Prelude.Mail qualified as Mail
import Gargantext.Prelude.NLP qualified as NLP
import Gargantext.System.Logging
import Gargantext.Utils.Jobs qualified as Jobs
import Gargantext.Utils.Jobs.Monad qualified as Jobs
import Gargantext.Utils.Jobs.Queue qualified as Jobs
import Gargantext.Utils.Jobs.Settings qualified as Jobs
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Prelude
import Servant.Auth.Client ()
import Servant.Client
import Servant.Job.Async qualified as ServantAsync
import Test.Database.Setup (withTestDB, fakeIniPath, testEnvToPgConnectionInfo)
import Test.Database.Types


newTestEnv :: TestEnv -> Logger (GargM Env BackendInternalError) -> Warp.Port -> IO Env
newTestEnv testEnv logger port = do
  file          <- fakeIniPath
  !manager_env  <- newTlsManager
  !settings'    <- devSettings devJwkFile <&> appPort .~ port

  !config_env    <- readConfig file
  prios          <- withLogger () $ \ioLogger -> Jobs.readPrios ioLogger (file <> ".jobs")
  let prios' = Jobs.applyPrios prios Jobs.defaultPrios
  !self_url_env  <- parseBaseUrl $ "http://0.0.0.0:" <> show port
  dbParam        <- pure $ testEnvToPgConnectionInfo testEnv
  !pool          <- newPool dbParam

  !nodeStory_env <- fromDBNodeStoryEnv pool
  !scrapers_env  <- ServantAsync.newJobEnv ServantAsync.defaultSettings manager_env

  secret        <- Jobs.genSecret
  let jobs_settings = (Jobs.defaultJobSettings 1 secret)
                        & Jobs.l_jsJobTimeout .~ (fromIntegral $ config_env ^. hasConfig ^. gc_js_job_timeout)
                        & Jobs.l_jsIDTimeout  .~ (fromIntegral $ config_env ^. hasConfig ^. gc_js_id_timeout)
  !jobs_env     <- Jobs.newJobEnv jobs_settings prios' manager_env
  !config_mail  <- Mail.readConfig file
  !nlp_env      <- nlpServerMap <$> NLP.readConfig file

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

withGargApp :: Application -> (Warp.Port -> IO ()) -> IO ()
withGargApp app action = do
  Warp.testWithApplication (pure app) action

withTestDBAndPort :: (((TestEnv, Warp.Port), Application) -> IO ()) -> IO ()
withTestDBAndPort action =
  withTestDB $ \testEnv -> do
    app <- withLoggerHoisted Mock $ \ioLogger -> do
             env <- newTestEnv testEnv ioLogger 8080
             makeApp env
    withGargApp app $ \port ->
      action ((testEnv, port), app)

setupEnvironment :: TestEnv -> IO ()
setupEnvironment env = flip runReaderT env $ runTestMonad $ do
  void $ initFirstTriggers "secret_key"
  void $ new_user $ mkNewUser (userMaster <> "@cnrs.com") (GargPassword "secret_key")
  (masterUserId, _masterRootId, masterCorpusId)
              <- getOrMkRootWithCorpus MkCorpusUserMaster (Nothing :: Maybe HyperdataCorpus)
  masterListId <- getOrMkList masterCorpusId masterUserId
  -- printDebug "[setupEnvironment] masterListId: " masterListId
  void $ initLastTriggers masterListId

-- | Creates two users, Alice & Bob. Alice shouldn't be able to see
-- Bob's private data and vice-versa.
createAliceAndBob :: TestEnv -> IO ()
createAliceAndBob testEnv = do
  void $ flip runReaderT testEnv $ runTestMonad $ do
    let nur1 = mkNewUser "alice@gargan.text" (GargPassword "alice")
    let nur2 = mkNewUser "bob@gargan.text" (GargPassword "bob")

    void $ new_user nur1
    void $ new_user nur2
