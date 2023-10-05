{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Test.API.Authentication (
    tests
  , auth_api
  ) where

import Prelude
import Data.Proxy
import Gargantext.API.Routes
import Network.HTTP.Client hiding (Proxy)
import Servant.Client
import Test.Hspec
import Test.Database.Types
import Servant.Auth.Client ()
import Gargantext.API.Admin.Auth.Types
import Gargantext.Core.Types.Individu
import Control.Monad
import Control.Monad.Reader
import Gargantext.Database.Action.User.New
import Gargantext.Core.Types
import Test.API.Setup (withTestDBAndPort, setupEnvironment)
import qualified Data.Text as T
import Control.Lens
import Data.Maybe

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
          Left err -> fail (show err)
          Right r  -> r `shouldSatisfy` ((>= 1) . T.length) -- we got something back

    describe "POST /api/v1.0/auth" $ do

      it "requires no auth and authenticates the user 'alice'" $ \((testEnv, port), _) -> do

        -- Let's create the Alice user.
        void $ flip runReaderT testEnv $ runTestMonad $ do
          void $ new_user $ mkNewUser "alice@gargan.text" (GargPassword "alice")

        let authPayload = AuthRequest "alice" (GargPassword "alice")
        result0 <- runClientM (auth_api authPayload) (clientEnv port)
        let result = over (_Right . authRes_valid . _Just . authVal_token) (const cannedToken) result0
        let expected = AuthResponse {
                         _authRes_valid = Just $
                           AuthValid {
                             _authVal_token = cannedToken
                           , _authVal_tree_id = fromMaybe (NodeId 1) $ listToMaybe $ result0 ^.. _Right . authRes_valid . _Just . authVal_tree_id
                           , _authVal_user_id = fromMaybe 1          $ listToMaybe $ result0 ^.. _Right . authRes_valid . _Just . authVal_user_id
                           }
                         , _authRes_inval = Nothing
                         }

        result `shouldBe` (Right expected)

      it "denies login for user 'alice' if password is invalid" $ \((_testEnv, port), _) -> do
        let authPayload = AuthRequest "alice" (GargPassword "wrong")
        result <- runClientM (auth_api authPayload) (clientEnv port)
        let expected = AuthResponse {
                       _authRes_valid = Nothing
                     , _authRes_inval = Just $ AuthInvalid "Invalid username or password"
                     }
        result `shouldBe` (Right expected)
