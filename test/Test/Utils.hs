{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Test.Utils where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Char (isSpace)
import Language.Haskell.TH.Quote
import Network.HTTP.Types
import Network.Wai.Test
import Prelude
import Test.Hspec.Expectations
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.Hspec.Wai.Matcher
import Test.Tasty.HUnit
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM

-- | Marks the input 'Assertion' as pending, by ignoring any exception
-- thrown by it.
pending :: String -> Assertion -> Assertion
pending reason act = act `catch` (\(e :: SomeException) -> do
  putStrLn $ "PENDING: " <> reason
  putStrLn (displayException e))

-- | Similar to 'json' from the 'Test.Hspec.Wai.JSON' package,
-- but allows matching on a /fragment/ of the body.
jsonFragment :: QuasiQuoter
jsonFragment = QuasiQuoter {
  quoteExp = \input -> [|fromValue $(quoteExp aesonQQ input)|]
, quotePat = const $ error "No quotePat defined for jsonFragment"
, quoteType = const $ error "No quoteType defined for jsonFragment"
, quoteDec = const $ error "No quoteDec defined for jsonFragment"
}

newtype JsonFragmentResponseMatcher = JsonFragmentResponseMatcher { getJsonMatcher :: ResponseMatcher }

shouldRespondWith' :: HasCallStack
                   => WaiSession st SResponse
                   -> JsonFragmentResponseMatcher
                   -> WaiExpectation st
shouldRespondWith' action matcher = do
  r <- action
  forM_ (match r (getJsonMatcher matcher)) (liftIO . expectationFailure)

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
      all (\(key, value) -> HM.lookup key sup == Just value) (HM.toList sub)
    isSubsetOf x y = x == y
