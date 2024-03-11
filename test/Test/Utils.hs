{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Utils where

import Control.Exception ()
import Control.Lens ((^.))
import Control.Monad ()
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as L
import Data.CaseInsensitive qualified as CI
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Fmt (Builder)
import Gargantext.API.Admin.Auth.Types (AuthRequest(..), Token, authRes_token)
import Gargantext.Core.Types.Individu (Username, GargPassword)
import Gargantext.Prelude
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Header, Method, status200)
import Network.HTTP.Types.Header (hAccept, hAuthorization, hContentType)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Test (SResponse(..))
import Prelude qualified
import Servant.Client (ClientEnv, baseUrlPort, defaultMakeClientRequest, makeClientRequest, mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Core.Request (addHeader)
import Test.API.Routes (auth_api, mkUrl)
import Test.Hspec.Expectations
import Test.Hspec.Wai (MatchBody(..), WaiExpectation, WaiSession, request)
import Test.Hspec.Wai.JSON (FromValue(..))
import Test.Hspec.Wai.Matcher (MatchHeader(..), ResponseMatcher(..), bodyEquals, formatHeader, match)
import Test.Tasty.HUnit (Assertion)
import Test.Types


-- | Marks the input 'Assertion' as pending, by ignoring any exception
-- thrown by it.
pending :: Prelude.String -> Assertion -> Assertion
pending reason act = act `catch` (\(e :: SomeException) -> do
  putStrLn $ "PENDING: " <> reason
  putStrLn (displayException e))


newtype JsonFragmentResponseMatcher = JsonFragmentResponseMatcher { getJsonMatcher :: ResponseMatcher }

-- | Succeeds if the full body matches the input /fragment/. Careful in using this
-- combinator, as it won't check that the full body matches the input, but rather
-- that the body contains the input fragment, which might lead to confusion.
shouldRespondWithFragment :: HasCallStack
                          => WaiSession st SResponse
                          -> JsonFragmentResponseMatcher
                          -> WaiExpectation st
shouldRespondWithFragment action matcher =
  shouldRespondWithFragmentCustomStatus 200 action matcher

-- | Same as above, but with custom status code
shouldRespondWithFragmentCustomStatus :: HasCallStack
                                      => Int
                                      -> WaiSession st SResponse
                                      -> JsonFragmentResponseMatcher
                                      -> WaiExpectation st
shouldRespondWithFragmentCustomStatus status action matcher = do
  let m = (getJsonMatcher matcher) { matchStatus = status }
  r <- action
  forM_ (match r (getJsonMatcher $ JsonFragmentResponseMatcher m)) (liftIO . expectationFailure)


instance FromValue JsonFragmentResponseMatcher where
  fromValue = JsonFragmentResponseMatcher . ResponseMatcher 200 [matchHeader] . containsJSON
    where
      matchHeader = MatchHeader $ \headers _body ->
        case Prelude.lookup "Content-Type" headers of
          Just h | isJSON h -> Nothing
          _ -> Just $ Prelude.unlines [
              "missing header:"
            , formatHeader ("Content-Type", "application/json")
            ]
      isJSON c = media == "application/json" && parameters `elem` ignoredParameters
        where
          (media, parameters) = let (m, p) = breakAt ';' c in (strip m, strip p)
          ignoredParameters = ["", "charset=utf-8"]

      breakAt c = fmap (B.drop 1) . B.break (== c)
      strip = B.reverse . B.dropWhile isSpace . B.reverse . B.dropWhile isSpace

shouldRespondWithJSON :: (JSON.FromJSON a, JSON.ToJSON a, HasCallStack)
                      => WaiSession st a
                      -> JsonFragmentResponseMatcher
                      -> WaiExpectation st
shouldRespondWithJSON action matcher = do
  r <- action
  forM_ (match (SResponse status200 mempty (JSON.encode r)) (getJsonMatcher matcher)) (liftIO . expectationFailure)

containsJSON :: JSON.Value -> MatchBody
containsJSON expected = MatchBody matcher
  where
    matcher headers actualBody = case JSON.decode actualBody of
      Just actual | expected `isSubsetOf` actual -> Nothing
      _ -> let MatchBody m = bodyEquals (JSON.encode expected) in m headers actualBody

    isSubsetOf :: JSON.Value -> JSON.Value -> Bool
    isSubsetOf (JSON.Object sub) (JSON.Object sup) =
      all (\(key, value) -> KM.lookup key sup == Just value) (KM.toList sub)
    isSubsetOf x y = x == y


authenticatedServantClient :: Int -> T.Text -> IO ClientEnv
authenticatedServantClient port token = do
  baseUrl <- parseBaseUrl "http://localhost"
  manager <- newManager defaultManagerSettings
  let requestAddToken url req =
          defaultMakeClientRequest url $ addHeader hAuthorization ("Bearer " <> token)
                                       $ addHeader hContentType (T.pack "application/json") req

  pure $ (mkClientEnv manager (baseUrl { baseUrlPort = port })) { makeClientRequest = requestAddToken }




-- | Issue a request with a valid 'Authorization: Bearer' inside.
protected :: HasCallStack
          => Token
          -> Method
          -> ByteString
          -> L.ByteString
          -> WaiSession () SResponse
protected tkn mth url = protectedWith mempty tkn mth url

protectedJSON :: forall a. (JSON.FromJSON a, Typeable a, HasCallStack)
              => Token
              -> Method
              -> ByteString
              -> JSON.Value
              -> WaiSession () a
protectedJSON tkn mth url = protectedJSONWith mempty tkn mth url

protectedJSONWith :: forall a. (JSON.FromJSON a, Typeable a, HasCallStack)
                  => [Header]
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

protectedWith :: HasCallStack
              => [Header]
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

protectedNewError :: HasCallStack => Token -> Method -> ByteString -> L.ByteString -> WaiSession () SResponse
protectedNewError tkn mth url = protectedWith newErrorFormat tkn mth url
  where
    newErrorFormat = [(CI.mk "X-Garg-Error-Scheme", "new")]

getJSON :: Token -> ByteString -> WaiSession () SResponse
getJSON tkn url = protectedWith mempty tkn "GET" url ""

postJSONUrlEncoded :: forall a. (JSON.FromJSON a, Typeable a, HasCallStack)
                   => Token
                   -> ByteString
                   -> L.ByteString
                   -> WaiSession () a
postJSONUrlEncoded tkn url queryPaths = do
  SResponse{..} <- protectedWith [(hContentType, "application/x-www-form-urlencoded")] tkn "POST" url queryPaths
  case JSON.eitherDecode simpleBody of
    Left err -> Prelude.fail $ "postJSONUrlEncoded failed when parsing " <> show (typeRep $ Proxy @a) <> ": " <> err <> "\nPayload was: " <> (T.unpack . TL.toStrict . TLE.decodeUtf8 $ simpleBody)
    Right x  -> pure x

withValidLogin :: (MonadFail m, MonadIO m) => Port -> Username -> GargPassword -> (Token -> m a) -> m a
withValidLogin port ur pwd act = do
  baseUrl <- liftIO $ parseBaseUrl "http://localhost"
  manager <- liftIO $ newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (baseUrl { baseUrlPort = port })
  let authPayload = AuthRequest ur pwd
  result <- liftIO $ runClientM (auth_api authPayload) clientEnv
  case result of
    Left err  -> liftIO $ throwIO $ Prelude.userError (show err)
    Right res -> act $ res ^. authRes_token


-- | Poll the given job URL every second until it finishes.
-- Retries up to 60 times (i.e. for 1 minute, before giving up)
pollUntilFinished :: HasCallStack
                  => Token
                  -> Port
                  -> (JobPollHandle -> Builder)
                  -> JobPollHandle
                  -> WaiSession () JobPollHandle
pollUntilFinished tkn port mkUrlPiece = go 60
  where
   go :: Int -> JobPollHandle -> WaiSession () JobPollHandle
   go 0 h  = panicTrace $ "pollUntilFinished exhausted attempts. Last found JobPollHandle: " <> TE.decodeUtf8 (L.toStrict $ JSON.encode h)
   go n h = case _jph_status h == "IsPending" || _jph_status h == "IsRunning" of
    True -> do
      liftIO $ threadDelay 1_000_000
      h' <- protectedJSON tkn "GET" (mkUrl port $ mkUrlPiece h) ""
      go (n-1) h'
    False
     | _jph_status h == "IsFailure"
     -> panicTrace $ "JobPollHandle contains a failure: " <> TE.decodeUtf8 (L.toStrict $ JSON.encode h)
     | otherwise
     -> pure h

