{-# LANGUAGE QuasiQuotes #-}
module Test.API.Errors (tests) where

import Gargantext.API.Routes
import Gargantext.Core.Types.Individu
import Gargantext.Prelude hiding (get)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types
import Network.Wai.Test
import Servant
import Servant.Auth.Client ()
import Servant.Auth.Client qualified as SA
import Servant.Client
import Test.API.Routes (mkUrl)
import Test.API.Setup (withTestDBAndPort, setupEnvironment, createAliceAndBob)
import Test.Hspec
import Test.Hspec.Wai.Internal (withApplication)
import Test.Utils (protected, withValidLogin, protectedNewError)
import Text.RawString.QQ (r)

tests :: Spec
tests = sequential $ aroundAll withTestDBAndPort $ do
  describe "Errors API" $ do
    describe "Prelude" $ do
      it "setup DB triggers and users" $ \((testEnv, port), _) -> do
        setupEnvironment testEnv
        baseUrl <- parseBaseUrl "http://localhost"
        manager <- newManager defaultManagerSettings
        let clientEnv prt = mkClientEnv manager (baseUrl { baseUrlPort = prt })

        createAliceAndBob testEnv

        let ( roots_api :<|> _nodes_api
              ) = client (Proxy :: Proxy (MkProtectedAPI GargAdminAPI)) (SA.Token "bogus")
        let ( admin_user_api_get :<|> _) = roots_api

        result <- runClientM admin_user_api_get (clientEnv port)
        length result `shouldBe` 0

    describe "GET /api/v1.0/node" $ do

      it "returns the old error by default" $ \((_testEnv, port), app) -> do
        withApplication app $ do
          withValidLogin port "gargantua" (GargPassword "secret_key") $ \token -> do
            res <- protected token "GET" (mkUrl port "/node/99") ""
            case res of
              SResponse{..}
                | Status{..} <- simpleStatus
                ->liftIO $ do
                    statusCode `shouldBe` 404
                    simpleBody `shouldBe` [r|{"error":"Node does not exist","node":99}|]

      it "returns the new error if header X-Garg-Error-Scheme: new is passed" $ \((_testEnv, port), app) -> do
        withApplication app $ do
          withValidLogin port "gargantua" (GargPassword "secret_key") $ \token -> do
            res <- protectedNewError token "GET" (mkUrl port "/node/99") ""
            case res of
              SResponse{..}
                | Status{..} <- simpleStatus
                ->liftIO $ do
                    statusCode `shouldBe` 404
                    simpleBody `shouldBe` [r|{"data":{"node_id":99},"diagnostic":"FE_node_lookup_failed_not_found {nenf_node_id = nodeId-99}","type":"EC_404__node_lookup_failed_not_found"}|]
