{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.API.Private (
    tests

  -- * Utility functions
  , withValidLogin
  , getJSON
  , protected
  , protectedJSON
  , postJSONUrlEncoded
  , protectedNewError
  , protectedWith
  ) where

import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as L
import Data.CaseInsensitive qualified as CI
import Data.Text.Encoding qualified as TE
import Gargantext.API.Admin.Auth.Types
import Gargantext.API.Routes
import Gargantext.Core.Types.Individu
import Gargantext.Prelude hiding (get)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types
import Network.Wai.Handler.Warp qualified as Wai
import Network.Wai.Test (SResponse (..))
import Prelude qualified
import Servant
import Servant.Auth.Client ()
import Servant.Auth.Client qualified as SA
import Servant.Client
import Test.API.Authentication (auth_api)
import Test.API.Setup (withTestDBAndPort, setupEnvironment, mkUrl, createAliceAndBob)
import Test.Hspec
import Test.Hspec.Wai hiding (pendingWith)
import Test.Hspec.Wai.Internal (withApplication)
import Test.Utils (jsonFragment, shouldRespondWith')
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as C8L

-- | Issue a request with a valid 'Authorization: Bearer' inside.
protected :: Token -> Method -> ByteString -> L.ByteString -> WaiSession () SResponse
protected tkn mth url = protectedWith mempty tkn mth url

protectedJSON :: forall a. (JSON.FromJSON a, Typeable a)
              => Token
              -> Method
              -> ByteString
              -> JSON.Value
              -> WaiSession () a
protectedJSON tkn mth url = protectedJSONWith mempty tkn mth url

protectedJSONWith :: forall a. (JSON.FromJSON a, Typeable a)
                  => [Network.HTTP.Types.Header]
                  -> Token
                  -> Method
                  -> ByteString
                  -> JSON.Value
                  -> WaiSession () a
protectedJSONWith hdrs tkn mth url jsonV = do
  SResponse{..} <- protectedWith hdrs tkn mth url (JSON.encode jsonV)
  case JSON.eitherDecode simpleBody of
    Left err -> Prelude.fail $ "protectedJSON failed when parsing " <> show (typeRep $ Proxy @a) <> ": " <> err
    Right x  -> pure x

protectedWith :: [Network.HTTP.Types.Header]
              -> Token
              -> Method -> ByteString -> L.ByteString -> WaiSession () SResponse
protectedWith extraHeaders tkn mth url payload =
  -- Using a map means that if any of the extra headers contains a clashing header name,
  -- the extra headers will take precedence.
  let defaultHeaders = [ (hAccept, "application/json;charset=utf-8")
                       , (hContentType, "application/json")
                       , (hAuthorization, "Bearer " <> TE.encodeUtf8 tkn)
                       ]
      hdrs = Map.toList $ Map.fromList $ defaultHeaders <> extraHeaders
  in request mth url hdrs payload

protectedNewError :: Token -> Method -> ByteString -> L.ByteString -> WaiSession () SResponse
protectedNewError tkn mth url = protectedWith newErrorFormat tkn mth url
  where
    newErrorFormat = [(CI.mk "X-Garg-Error-Scheme", "new")]

getJSON :: ByteString -> WaiSession () SResponse
getJSON url =
  request "GET" url [(hContentType, "application/json")] ""

postJSONUrlEncoded :: forall a. (JSON.FromJSON a, Typeable a)
                   => Token
                   -> ByteString
                   -> L.ByteString
                   -> WaiSession () a
postJSONUrlEncoded tkn url queryPaths = do
  SResponse{..} <- protectedWith [(hContentType, "application/x-www-form-urlencoded")] tkn "POST" url queryPaths
  case JSON.eitherDecode simpleBody of
    Left err -> Prelude.fail $ "postJSONUrlEncoded failed when parsing " <> show (typeRep $ Proxy @a) <> ": " <> err <> "\nPayload was: " <> (C8L.unpack simpleBody)
    Right x  -> pure x

withValidLogin :: (MonadFail m, MonadIO m) => Wai.Port -> Username -> GargPassword -> (Token -> m a) -> m a
withValidLogin port ur pwd act = do
  baseUrl <- liftIO $ parseBaseUrl "http://localhost"
  manager <- liftIO $ newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (baseUrl { baseUrlPort = port })
  let authPayload = AuthRequest ur pwd
  result <- liftIO $ runClientM (auth_api authPayload) clientEnv
  case result of
    Left err  -> liftIO $ throwIO $ Prelude.userError (show err)
    Right res
      | Just tkn <- _authRes_valid res
      -> act (_authVal_token tkn)
      | otherwise
      -> Prelude.fail $ "No token found in " <> show res


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
              `shouldRespondWith'` [jsonFragment| {"id":8,"user_id":2,"name":"alice" } |]

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
              `shouldRespondWith'` [jsonFragment| { "node": {"id":8, "name":"alice", "type": "NodeUser" } } |]

      it "forbids 'alice' to see others node private info" $ \((_testEnv, port), app) -> do
        withApplication app $ do
          withValidLogin port "alice" (GargPassword "alice") $ \token -> do
            protected token "GET" (mkUrl port "/tree/1") "" `shouldRespondWith` 403
