{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.API.Private where

import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Data.Proxy
import Gargantext.API.Admin.Auth.Types
import Gargantext.API.Routes
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.User.New
import Network.HTTP.Client hiding (Proxy)
import Prelude
import Servant
import Servant.Auth.Client ()
import Servant.Client
import Test.API.Authentication (auth_api)
import Test.API.Setup (withTestDBAndPort)
import Test.Database.Types
import Test.Hspec
import qualified Data.Text.Encoding as TE
import qualified Servant.Auth.Client as SA

tests :: Spec
tests = sequential $ aroundAll withTestDBAndPort $ do
  describe "Private API" $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    describe "GET /api/v1.0/user" $ do

      -- FIXME(adn): unclear if this is useful at all. Doesn't do permission checking.
      it "doesn't allow someone with an invalid token to show the results" $ \(testEnv, port) -> do

        -- Let's create two users, Alice & Bob. Alice shouldn't be able to see
        -- Bob's private data and vice-versa.
        void $ flip runReaderT testEnv $ runTestMonad $ do
          let nur1 = mkNewUser "alice@gargan.text" (GargPassword "alice")
          let nur2 = mkNewUser "bob@gargan.text" (GargPassword "bob")

          void $ new_user nur1
          void $ new_user nur2

        let ( roots_api :<|> _nodes_api
              ) = client (Proxy :: Proxy (MkProtectedAPI GargAdminAPI)) (SA.Token "bogus")
        let ( admin_user_api_get :<|> _) = roots_api

        result <- runClientM admin_user_api_get (clientEnv port)
        length result `shouldBe` 0

      -- FIXME(adn): unclear if this is useful at all. Doesn't do permission checking.
      it "allows 'alice' to see the results" $ \(_testEnv, port) -> do

        let authPayload = AuthRequest "alice" (GargPassword "alice")
        Right result <- runClientM (auth_api authPayload) (clientEnv port)

        let token = _authVal_token $ fromJust (_authRes_valid result)

        let ( roots_api :<|> _nodes_api
              ) = client (Proxy :: Proxy (MkProtectedAPI GargAdminAPI)) (SA.Token $ TE.encodeUtf8 $ token)
        let ( admin_user_api_get :<|> _) = roots_api

        _nodes <- runClientM admin_user_api_get (clientEnv port)
        pendingWith "currently useless"
