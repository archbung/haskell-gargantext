{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.Date
Description : Some utils to parse dates
Copyright   : (c) CNRS 2017-present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

According to the language of the text, parseDateRaw returns date as Text:

TODO : Add some tests
import Gargantext.Core.Text.Corpus.Parsers.Date as DGP
DGP.parseDateRaw DGP.FR "12 avril 2010" == "2010-04-12T00:00:00.000+00:00"
-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE TypeFamilies #-}

module Gargantext.Core.Text.Corpus.Parsers.Date
{-(parse, parseRaw, dateSplit, Year, Month, Day)-}
  where

import Data.Aeson (toJSON, Value)
import Data.Aeson qualified as Json
import Data.HashMap.Strict as HM hiding (map)
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Text (unpack, splitOn, replace)
import Data.Time (defaultTimeLocale, iso8601DateFormat, parseTimeM, toGregorian)
import Data.Time.Calendar qualified as DTC
import Data.Time.Clock ( secondsToDiffTime)
import Data.Time.Clock (UTCTime(..))  -- , getCurrentTime)
import Data.Time.LocalTime (utc)
import Data.Time.LocalTime.TimeZone.Series (zonedTimeToZoneSeriesTime)
import Duckling.Api (analyze)
import Duckling.Core (makeLocale, Dimension(Time))
import Duckling.Core qualified as DC
import Duckling.Resolve (fromUTC, Context(Context, referenceTime, locale), DucklingTime(DucklingTime), Options(..))
import Duckling.Types (ResolvedToken(..), ResolvedVal(..))
import Duckling.Types (Seal(..))
import Gargantext.Core (Lang(FR,EN))
-- import Gargantext.Core.Types (DebugMode(..), withDebugMode)
import Gargantext.Prelude hiding (replace)
import System.Environment (getEnv)
------------------------------------------------------------------------
-- | Parse date to Ints
-- TODO add hours, minutes and seconds
dateSplit :: Text -> Either Text (UTCTime, (Year, Month, Day))
dateSplit txt = mkSplit <$> parse txt
  where
    mkSplit utcTime =
      let (y, m, d) = split' utcTime in
      (utcTime, (y, m, d))

mDateSplit :: Maybe Text -> (Maybe UTCTime, (Maybe Year, Maybe Month, Maybe Day))
mDateSplit Nothing = (Nothing, (Nothing, Nothing, Nothing))
mDateSplit (Just md) =
  case dateSplit md of
    Left _err -> (Nothing, (Nothing, Nothing, Nothing))
    Right (ut, (y, m, d)) -> (Just ut, (Just y, Just m, Just d))


split' :: UTCTime -> (Year, Month, Day)
split' (UTCTime day _) = (fromIntegral y, m, d)
  where
    (y,m,d)         = toGregorian day

type Year  = Int
type Month = Int
type Day   = Int
------------------------------------------------------------------------

-- | Date Parser
-- Parses dates mentions in full text given the language.
-- >>> parse FR (pack "1 avril 1900 à 19H")
-- 1900-04-01 19:00:00 UTC
-- >>> parse EN (pack "April 1 1900")
-- 1900-04-01 00:00:00 UTC
parse :: Text -> Either Text UTCTime
parse s = do
  -- printDebug "Date: " s
  let result = dateFlow (DucklingFailure s)
  --printDebug "Date': " dateStr'
  case result of
    DateFlowSuccess ok -> Right ok
    DateFlowFailure    -> Left "[G.C.T.C.Parsers.Date] DateFlowFailure"
    -- DateFlowFailure    -> (withDebugMode (DebugMode True)
    --                                     "[G.C.T.P.T.Date parse]" s
    --                                     $ getCurrentTime)
    _                   -> Left "[G.C.T.C.Parsers.Date] parse: Should not happen"

defaultDate :: Text
defaultDate = "0-0-0T0:0:0"

type DateFormat  = Text
type DateDefault = Text


data DateFlow = DucklingSuccess { ds_result  :: Text }
              | DucklingFailure { df_result  :: Text }
              | ReadFailure1    { rf1_result :: Text }
              | ReadFailure2    { rf2_result :: Text }
              | DateFlowSuccess { success :: UTCTime }
              | DateFlowFailure
  deriving Show

--{-
dateFlow :: DateFlow -> DateFlow
dateFlow (DucklingSuccess res) = case (head $ splitOn "." res)  of
                             Nothing -> dateFlow (ReadFailure1 res)
                             Just re -> case readDate res of
                                Nothing -> dateFlow (ReadFailure1 re)
                                Just ok -> DateFlowSuccess ok
--dateFlow (DucklingFailure txt) = case readDate $ replace " " "T" txt of
dateFlow (DucklingFailure txt) = case readDate  (fromMaybe "" $ headMay $ List.filter (/= "") $ splitOn " " txt) of
                             Nothing -> dateFlow (ReadFailure1 txt)
                             Just ok -> DateFlowSuccess ok
dateFlow (ReadFailure1 txt) = case readDate txt of
                          Nothing -> dateFlow $ ReadFailure2 txt
                          Just ok -> DateFlowSuccess ok
dateFlow (ReadFailure2 txt) = case readDate $ replace " " "" txt <> "-01-01" of
                          Nothing -> DateFlowFailure
                          Just ok -> DateFlowSuccess ok
dateFlow _ = DateFlowFailure
--}

readDate :: Text -> Maybe UTCTime
readDate txt = do
  --let format = cs $ iso8601DateFormat (Just "%F %H:%M:%S")
  let format = cs $ iso8601DateFormat Nothing
  parseTimeM True defaultTimeLocale (unpack format) (cs txt)


-- TODO add Paris at Duckling.Locale Region datatype
-- | To get Homogeinity of the languages
--   TODO : put this in a more generic place in the source code
parserLang :: Lang -> DC.Lang
parserLang FR    = DC.FR
parserLang EN    = DC.EN
parserLang lang  = panic $ "[G.C.T.C.P.Date] Lang not implemented" <> (show lang)

-- | Final Date parser API
-- IO can be avoided here:
-- currentContext :: Lang -> IO Context
-- currentContext lang = localContext lang <$> utcToDucklingTime <$> getCurrentTime
-- parseRaw :: Context -> Text -> SomeErrorHandling Text


parseRawSafe :: Lang -> Text -> IO DateFlow
parseRawSafe lang text = do
  let triedParseRaw = parseRaw lang text
  dateStr' <- case triedParseRaw of
      --Left (CE.SomeException err) -> do
      Left _err -> do
        _envLang <- getEnv "LANG"
        -- printDebug "[G.C.T.C.P.Date] Exception: " (err, envLang, lang, text)
        pure $ DucklingFailure text
      Right res -> pure $ DucklingSuccess res
  pure dateStr'

--tryParseRaw :: CE.Exception e => Lang -> Text -> IO (Either e Text)
--tryParseRaw lang text = CE.try (parseRaw lang text)

parseRaw :: Lang -> Text -> Either Text Text
parseRaw lang text = do -- case result
    let maybeResult = extractValue $ getTimeValue
                                   $ parseDateWithDuckling lang text (Options True)
    case maybeResult of
      Just result -> Right result
      Nothing     -> do
        -- printDebug ("[G.C.T.C.P.D.parseRaw] ERROR " <> (cs . show) lang) text
        Left $ "[G.C.T.C.P.D.parseRaw ERROR] " <> show lang <> " :: " <> text

getTimeValue :: [ResolvedToken] -> Maybe Value
getTimeValue rt = case head rt of
  Nothing -> do
    Nothing
  Just x  -> case rval x of
    RVal Time t -> Just $ toJSON t
    _  -> do
      Nothing

extractValue :: Maybe Value -> Maybe Text
extractValue (Just (Json.Object object)) =
  case HM.lookup "value" object of
    Just (Json.String date) -> Just date
    _                  -> Nothing
extractValue _ = Nothing

-- | Current Time in DucklingTime format
-- TODO : get local Time in a more generic way
utcToDucklingTime :: UTCTime -> DucklingTime
utcToDucklingTime time = DucklingTime . zonedTimeToZoneSeriesTime $ fromUTC time utc

-- | Local Context which depends on Lang and Time
localContext :: Lang -> DucklingTime -> Context
localContext lang dt = Context { referenceTime = dt
                               , locale = makeLocale (parserLang lang) Nothing }

defaultDay :: DTC.Day
defaultDay = DTC.fromGregorian 1 1 1

defaultUTCTime :: UTCTime
defaultUTCTime = UTCTime { utctDay = defaultDay
                         , utctDayTime = secondsToDiffTime 0 }

-- | Date parser with Duckling
parseDateWithDuckling :: Lang -> Text -> Options -> [ResolvedToken]
parseDateWithDuckling lang input options = do
  let contxt = localContext lang $ utcToDucklingTime defaultUTCTime
  --pure $ parseAndResolve (rulesFor (locale ctx) (HashSet.fromList [(This Time)])) input ctx
  -- TODO check/test Options False or True
  analyze input contxt options $ HashSet.fromList [(Seal Time)]
