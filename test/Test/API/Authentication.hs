{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Test.API.Authentication where

import Prelude
import Data.Proxy
import Gargantext.API (makeApp)
import Gargantext.API.Admin.EnvTypes (Mode(Mock), Env (..))
import Gargantext.API.Admin.Settings
import Gargantext.API.Routes
import Gargantext.System.Logging
import Network.HTTP.Client hiding (Proxy)
import Servant.Client
import Test.Database.Setup (withTestDB, fakeIniPath, testEnvToPgConnectionInfo)
import Test.Hspec
import qualified Network.Wai.Handler.Warp         as Warp
import Test.Database.Types
import Gargantext.API.Prelude
import qualified Gargantext.Utils.Jobs as Jobs
import qualified Gargantext.Utils.Jobs.Queue as Jobs
import qualified Gargantext.Utils.Jobs.Settings as Jobs
import qualified Gargantext.Utils.Jobs.Monad as Jobs
import qualified Gargantext.Prelude.Mail as Mail
import qualified Gargantext.Prelude.NLP as NLP
import Network.HTTP.Client.TLS (newTlsManager)
import Control.Lens
import Gargantext.API.Admin.Types
import Gargantext.Prelude.Config
import Gargantext.Core.NodeStory
import Gargantext.Database.Prelude
import Gargantext.Core.NLP
import qualified Servant.Job.Async as ServantAsync
import Servant.Auth.Client ()
import Gargantext.API.Admin.Auth.Types
import Gargantext.Core.Types.Individu
import Control.Monad
import Control.Monad.Reader
import Gargantext.Database.Action.User.New
import Gargantext.Core.Types

newTestEnv :: TestEnv -> Logger (GargM Env GargError) -> Warp.Port -> IO Env
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

  !nodeStory_env <- readNodeStoryEnv pool
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

withGargApp :: TestEnv -> (Warp.Port -> IO ()) -> IO ()
withGargApp testEnv action = do
  let createApp = do
        withLoggerHoisted Mock $ \ioLogger -> do
          env <- newTestEnv testEnv ioLogger 8080
          makeApp env
  Warp.testWithApplication createApp action

withTestDBAndPort :: ((TestEnv, Warp.Port) -> IO ()) -> IO ()
withTestDBAndPort action =
  withTestDB $ \testEnv ->
    withGargApp testEnv $ \port ->
      action (testEnv, port)

tests :: Spec
tests = sequential $ aroundAll withTestDBAndPort $ do
  describe "Authentication" $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    -- testing scenarios start here
    describe "GET /api/v1.0/version" $ do
      let version_api = client (Proxy :: Proxy (MkGargAPI (GargAPIVersion GargVersion)))
      it "requires no auth and returns the current version" $ \(_testEnv, port) -> do
        result <- runClientM version_api (clientEnv port)
        result `shouldBe` (Right "0.0.6.9.9.7.7")

    describe "POST /api/v1.0/auth" $ do
      let auth_api = client (Proxy :: Proxy (MkGargAPI (GargAPIVersion AuthAPI)))

      it "requires no auth and authenticates the user 'alice'" $ \(testEnv, port) -> do

        -- Let's create two users, Alice & Bob. Alice shouldn't be able to see
        -- Bob's private data and vice-versa.
        void $ flip runReaderT testEnv $ runTestMonad $ do
          let nur1 = mkNewUser "alice@gargan.text" (GargPassword "alice")
          let nur2 = mkNewUser "bob@gargan.text" (GargPassword "bob")

          void $ new_user nur1
          void $ new_user nur2

        let authPayload = AuthRequest "alice" (GargPassword "alice")
        result <- runClientM (auth_api authPayload) (clientEnv port)
        let expected = AuthResponse {
                         _authRes_valid = Just $
                           AuthValid {
                             _authVal_token = "eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiaWQiOjF9fQ.t49zZSqkPAulEkYEh4pW17H2uwrkyPTdZKwHyG3KUJ0hzU2UUoPBNj8vdv087RCVBJ4tXgxNbP4j0RBv3gxdqg"
                           , _authVal_tree_id = NodeId 1
                           , _authVal_user_id = 1
                           }
                         , _authRes_inval = Nothing
                         }
        result `shouldBe` (Right expected)

      it "denies login for user 'alice' if password is invalid" $ \(_testEnv, port) -> do
        let authPayload = AuthRequest "alice" (GargPassword "wrong")
        result <- runClientM (auth_api authPayload) (clientEnv port)
        let expected = AuthResponse {
                       _authRes_valid = Nothing
                     , _authRes_inval = Just $ AuthInvalid "Invalid password"
                     }
        result `shouldBe` (Right expected)
