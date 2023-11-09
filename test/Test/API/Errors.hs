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
import Servant.Client
import Test.API.Private (protected, withValidLogin)
import Test.API.Setup (withTestDBAndPort, setupEnvironment, mkUrl, createAliceAndBob)
import Test.Hspec
import Test.Hspec.Wai.Internal (withApplication)
import Text.RawString.QQ (r)
import qualified Servant.Auth.Client as SA

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
                    simpleBody `shouldBe` [r|{"error":"Node does not exist (nodeId-99)"}|]
