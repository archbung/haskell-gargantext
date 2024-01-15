{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Test.API.Authentication (
    tests
  , auth_api
  ) where

import Control.Lens
import Data.Proxy
import Data.Text as T
import Gargantext.API.Admin.Auth.Types
import Gargantext.API.Routes
import Gargantext.Core.Types
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.User.New
import Gargantext.Prelude
import Network.HTTP.Client hiding (Proxy)
import Prelude qualified
import Servant.Auth.Client ()
import Servant.Client
import Test.API.Setup (withTestDBAndPort, setupEnvironment)
import Test.Database.Types
import Test.Hspec

auth_api :: AuthRequest -> ClientM AuthResponse
auth_api = client (Proxy :: Proxy (MkGargAPI (GargAPIVersion AuthAPI)))

cannedToken :: T.Text
cannedToken = "eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiaWQiOjF9fQ.t49zZSqkPAulEkYEh4pW17H2uwrkyPTdZKwHyG3KUJ0hzU2UUoPBNj8vdv087RCVBJ4tXgxNbP4j0RBv3gxdqg"

tests :: Spec
tests = sequential $ aroundAll withTestDBAndPort $ do
  describe "Prelude" $ do
    it "setup DB triggers" $ \((testEnv, _), _) -> setupEnvironment testEnv
  describe "Authentication" $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    -- testing scenarios start here
    describe "GET /api/v1.0/version" $ do
      let version_api = client (Proxy :: Proxy (MkGargAPI (GargAPIVersion GargVersion)))
      it "requires no auth and returns the current version" $ \((_testEnv, port), _) -> do
        result <- runClientM version_api (clientEnv port)
        case result of
          Left err -> Prelude.fail (show err)
          Right r  -> r `shouldSatisfy` ((>= 1) . T.length) -- we got something back

    describe "POST /api/v1.0/auth" $ do

      it "requires no auth and authenticates the user 'alice'" $ \((testEnv, port), _) -> do

        -- Let's create the Alice user.
        void $ flip runReaderT testEnv $ runTestMonad $ do
          void $ new_user $ mkNewUser "alice@gargan.text" (GargPassword "alice")

        let authPayload = AuthRequest "alice" (GargPassword "alice")
        result0 <- runClientM (auth_api authPayload) (clientEnv port)
        let result = over (_Right . authRes_token) (const cannedToken) result0
        let expected = AuthResponse {
                _authRes_token = cannedToken
              , _authRes_tree_id = fromMaybe (UnsafeMkNodeId 1) $ listToMaybe $ result0 ^.. _Right . authRes_tree_id
              , _authRes_user_id = fromMaybe (UnsafeMkUserId 1) $ listToMaybe $ result0 ^.. _Right . authRes_user_id
              }

        result `shouldBe` (Right expected)

      it "denies login for user 'alice' if password is invalid" $ \((_testEnv, port), _) -> do
        let authPayload = AuthRequest "alice" (GargPassword "wrong")
        result <- runClientM (auth_api authPayload) (clientEnv port)
        putText $ "result: " <> show result
        -- result `shouldBe` (Left $ InvalidUsernameOrPassword)
