{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Test.Utils where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as B
import Data.Char (isSpace)
import Network.HTTP.Types
import Network.Wai.Test
import Prelude
import Test.Hspec.Expectations
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON (FromValue(..))
import Test.Hspec.Wai.Matcher
import Test.Tasty.HUnit

-- | Marks the input 'Assertion' as pending, by ignoring any exception
-- thrown by it.
pending :: String -> Assertion -> Assertion
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
        case lookup "Content-Type" headers of
          Just h | isJSON h -> Nothing
          _ -> Just $ unlines [
              "missing header:"
            , formatHeader ("Content-Type", "application/json")
            ]
      isJSON c = media == "application/json" && parameters `elem` ignoredParameters
        where
          (media, parameters) = let (m, p) = breakAt ';' c in (strip m, strip p)
          ignoredParameters = ["", "charset=utf-8"]

      breakAt c = fmap (B.drop 1) . B.break (== c)
      strip = B.reverse . B.dropWhile isSpace . B.reverse . B.dropWhile isSpace

shouldRespondWithJSON :: (FromJSON a, ToJSON a, HasCallStack)
                      => WaiSession st a
                      -> JsonFragmentResponseMatcher
                      -> WaiExpectation st
shouldRespondWithJSON action matcher = do
  r <- action
  forM_ (match (SResponse status200 mempty (JSON.encode r)) (getJsonMatcher matcher)) (liftIO . expectationFailure)

containsJSON :: Value -> MatchBody
containsJSON expected = MatchBody matcher
  where
    matcher headers actualBody = case decode actualBody of
      Just actual | expected `isSubsetOf` actual -> Nothing
      _ -> let MatchBody m = bodyEquals (encode expected) in m headers actualBody

    isSubsetOf :: Value -> Value -> Bool
    isSubsetOf (Object sub) (Object sup) =
      all (\(key, value) -> KM.lookup key sup == Just value) (KM.toList sub)
    isSubsetOf x y = x == y
