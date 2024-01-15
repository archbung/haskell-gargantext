{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

{-| Edit 'sensitiveKeywords' to extend the list of redacted fields. -}
module Gargantext.API.Middleware (
  logStdoutDevSanitised
  ) where

import Control.Lens
import Control.Monad.Logger
import Data.Aeson qualified as A
import Data.Aeson.Lens qualified as L
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.CaseInsensitive qualified as CI
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Prelude
import System.Console.ANSI

-- | Like 'logStdoutDev' from \"wai-extra\", but redacts (or omits altogether) payloads which might have
-- sensitive information
logStdoutDevSanitised :: IO Middleware
logStdoutDevSanitised = mkRequestLogger $ defaultRequestLoggerSettings { outputFormat = CustomOutputFormatWithDetailsAndHeaders customOutput }
-- |
-- Like 'key', but uses 'at' instead of 'ix'. This is handy when
-- adding and removing object keys:
--
-- >>> "{\"a\": 100, \"b\": 200}" & atKey "a" .~ Nothing
-- "{\"b\":200}"
--
-- >>> "{\"a\": 100, \"b\": 200}" & atKey "c" ?~ String "300"
-- "{\"a\":100,\"b\":200,\"c\":\"300\"}"
atKey :: L.AsValue t => T.Text -> Traversal' t (Maybe A.Value)
atKey i = L._Object . at i
{-# INLINE atKey #-}

customOutput :: OutputFormatterWithDetailsAndHeaders
customOutput _zonedDate rq status _mb_response_size request_dur (sanitiseBody . mconcat -> reqbody) _raw_response (map sanitiseHeader -> headers) =
  let params = map sanitiseQueryItem (queryString rq)
  in mkRequestLog params reqbody <> mkResponseLog

  where

    mkRequestLog :: [QueryItem] -> ByteString -> LogStr
    mkRequestLog params bdy =
        foldMap toLogStr (ansiMethod' (requestMethod rq))
            <> " "
            <> toLogStr (rawPathInfo rq)
            <> "\n"
            <> foldMap (\(k, mb_v) -> toLogStr $ show (k, mb_v)) params
            <> toLogStr bdy
            <> "\n"
            <> foldMap (\(k, v) -> toLogStr $ mconcat $ ansiColor' White $ "  " <> CI.original k <> ": " <> v <> "\n") headers
            <> "\n"

    mkResponseLog :: LogStr
    mkResponseLog =
      foldMap toLogStr (ansiColor' White "  Status: ")
          <> foldMap toLogStr (ansiStatusCode' status (C8.pack (show $ statusCode status) <> " " <> statusMessage status))
          <> " "
          <> "Served in " <> toLogStr (C8.pack $ show $ request_dur)
          <> "\n"

sanitiseBody :: ByteString -> ByteString
sanitiseBody blob = L.foldr (\k acc -> over (atKey k) (updateField k) acc) blob sensitiveKeywords
  where
    updateField :: T.Text -> Maybe A.Value -> Maybe A.Value
    updateField _ Nothing = Nothing
    updateField k (Just x)
      | A.String _v <- x
      , k `elem` sensitiveKeywords
      = Just $ A.String "*****"
      | otherwise
      = Just x

sanitiseQueryItem :: QueryItem -> QueryItem
sanitiseQueryItem (k, mb_v)
  | TE.decodeUtf8 k `elem` sensitiveKeywords
  = (k, (\v -> if C8.null v then mempty else "*****") <$> mb_v)
  | otherwise
  = (k, mb_v)

-- /NOTE:/ Extend this list to filter for more sensitive keywords.
sensitiveKeywords :: [T.Text]
sensitiveKeywords = [
    "password"
  , "api_key"
  , "apiKey"
  , "pubmedAPIKey"
  ]


sanitiseHeader :: Header -> Header
sanitiseHeader (hName, content)
  | hName == hAuthorization = (hName, "*****")
  | hName == hCookie        = (hName, "*****")
  | hName == hSetCookie     = (hName, "*****")
  | otherwise               = (hName, content)

ansiColor' :: Color -> BS.ByteString -> [BS.ByteString]
ansiColor' color bs =
    [ C8.pack $ setSGRCode [SetColor Foreground Dull color]
    , bs
    , C8.pack $ setSGRCode [Reset]
    ]

-- | Tags http method with a unique color.
ansiMethod' :: BS.ByteString -> [BS.ByteString]
ansiMethod' m = case m of
    "GET" -> ansiColor' Cyan m
    "HEAD" -> ansiColor' Cyan m
    "PUT" -> ansiColor' Green m
    "POST" -> ansiColor' Yellow m
    "DELETE" -> ansiColor' Red m
    _ -> ansiColor' Magenta m

ansiStatusCode' :: Status -> ByteString -> [BS.ByteString]
ansiStatusCode' (Status c _) t = case C8.take 1 (C8.pack . show $ c) of
    "2" -> ansiColor' Green t
    "3" -> ansiColor' Yellow t
    "4" -> ansiColor' Red t
    "5" -> ansiColor' Magenta t
    _ -> ansiColor' Blue t
