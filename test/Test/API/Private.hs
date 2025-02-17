{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.API.Private (
    tests
  ) where

import Data.Text.Encoding qualified as TE
import Gargantext.API.Routes
import Gargantext.Core.Types.Individu
import Gargantext.Prelude hiding (get)
import Network.HTTP.Client hiding (Proxy)
import Servant
import Servant.Auth.Client ()
import Servant.Auth.Client qualified as SA
import Servant.Client
import Test.API.Routes (mkUrl)
import Test.API.Setup (withTestDBAndPort, setupEnvironment, createAliceAndBob)
import Test.Hspec
import Test.Hspec.Wai hiding (pendingWith)
import Test.Hspec.Wai.Internal (withApplication)
import Test.Hspec.Wai.JSON (json)
import Test.Utils (protected, shouldRespondWithFragment, withValidLogin)


tests :: Spec
tests = sequential $ aroundAll withTestDBAndPort $ do
  describe "Prelude" $ do
    it "setup DB triggers" $ \((testEnv, _), _) -> setupEnvironment testEnv
  describe "Private API" $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    describe "GET /api/v1.0/user" $ do

      -- FIXME(adn): unclear if this is useful at all. Doesn't do permission checking.
      it "doesn't allow someone with an invalid token to show the results" $ \((testEnv, port), _) -> do

        createAliceAndBob testEnv

        let ( roots_api :<|> _nodes_api
              ) = client (Proxy :: Proxy (MkProtectedAPI GargAdminAPI)) (SA.Token "bogus")
        let ( admin_user_api_get :<|> _) = roots_api

        result <- runClientM admin_user_api_get (clientEnv port)
        length result `shouldBe` 0

      -- FIXME(adn): unclear if this is useful at all. Doesn't do permission checking.
      it "allows 'alice' to see the results" $ \((_testEnv, port), _) -> do

        withValidLogin port "alice" (GargPassword "alice") $ \token -> do
          let ( roots_api :<|> _nodes_api
                ) = client (Proxy :: Proxy (MkProtectedAPI GargAdminAPI)) (SA.Token $ TE.encodeUtf8 $ token)
          let ( admin_user_api_get :<|> _) = roots_api

          _nodes <- runClientM admin_user_api_get (clientEnv port)
          pendingWith "currently useless"

    describe "GET /api/v1.0/node" $ do

      it "unauthorised users shouldn't see anything" $ \((_testEnv, port), app) -> do
        withApplication app $ do
          get (mkUrl port "/node/1") `shouldRespondWith` 401

      it "allows 'alice' to see her own node info" $ \((_testEnv, port), app) -> do
        withApplication app $ do
          withValidLogin port "alice" (GargPassword "alice") $ \token -> do
            protected token "GET" (mkUrl port "/node/8") ""
              `shouldRespondWithFragment` [json| {"id":8,"user_id":2,"name":"alice" } |]

      it "forbids 'alice' to see others node private info" $ \((_testEnv, port), app) -> do
        withApplication app $ do
          withValidLogin port "alice" (GargPassword "alice") $ \token -> do
            protected token "GET" (mkUrl port "/node/1") "" `shouldRespondWith` 403

    describe "GET /api/v1.0/tree" $ do
      it "unauthorised users shouldn't see anything" $ \((_testEnv, port), app) -> do
        withApplication app $ do
          get (mkUrl port "/tree/1") `shouldRespondWith` 401

      it "allows 'alice' to see her own node info" $ \((_testEnv, port), app) -> do
        withApplication app $ do
          withValidLogin port "alice" (GargPassword "alice") $ \token -> do
            protected token "GET" (mkUrl port "/tree/8") ""
              `shouldRespondWithFragment` [json| { "node": {"id":8, "name":"alice", "type": "NodeUser" } } |]

      it "forbids 'alice' to see others node private info" $ \((_testEnv, port), app) -> do
        withApplication app $ do
          withValidLogin port "alice" (GargPassword "alice") $ \token -> do
            protected token "GET" (mkUrl port "/tree/1") "" `shouldRespondWith` 403
