{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.API.Private where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Data.Proxy
import Fmt
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
import Test.Hspec.Wai hiding (pendingWith)
import Test.Hspec.Wai.Internal (withApplication)
import Test.Hspec.Wai.JSON
import qualified Data.Text.Encoding as TE
import qualified Network.Wai.Handler.Warp as Wai
import qualified Servant.Auth.Client as SA
import Data.ByteString (ByteString)
import Network.Wai.Test (SResponse)
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as L

type Env = ((TestEnv, Wai.Port), Application)

curApi :: Builder
curApi = "v1.0"

mkUrl :: Wai.Port -> Builder -> ByteString
mkUrl _port urlPiece =
  "/api/" +| curApi |+ urlPiece

-- | Issue a request with a valid 'Authorization: Bearer' inside.
protected :: Token -> Method -> ByteString -> L.ByteString -> WaiSession () SResponse
protected tkn mth url payload =
  request mth url [ (hAccept, "application/json;charset=utf-8")
                  , (hContentType, "application/json")
                  , (hAuthorization, TE.encodeUtf8 tkn)
                  ] payload

getJSON :: ByteString -> WaiSession () SResponse
getJSON url =
  request "GET" url [(hContentType, "application/json")] ""

withValidLogin :: MonadIO m => Wai.Port -> Username -> GargPassword -> (Token -> m a) -> m a
withValidLogin port ur pwd act = do
  baseUrl <- liftIO $ parseBaseUrl "http://localhost"
  manager <- liftIO $ newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (baseUrl { baseUrlPort = port })
  let authPayload = AuthRequest ur pwd
  result <- liftIO $ runClientM (auth_api authPayload) clientEnv
  case result of
    Left err  -> liftIO $ throwIO $ userError (show err)
    Right res -> let token = _authVal_token $ fromJust (_authRes_valid res) in act token


tests :: Spec
tests = sequential $ aroundAll withTestDBAndPort $ do
  describe "Private API" $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    describe "GET /api/v1.0/user" $ do

      -- FIXME(adn): unclear if this is useful at all. Doesn't do permission checking.
      it "doesn't allow someone with an invalid token to show the results" $ \((testEnv, port), _) -> do

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
            protected token "GET" (mkUrl port "/node/1") "" `shouldRespondWith` [json| { } |]

