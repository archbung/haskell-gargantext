{-# LANGUAGE  LambdaCase #-}

{-|
Module      : Gargantext.Core.Text.Corpus.API.Istex
Description : Pubmed API connection
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Core.Text.Corpus.API.Istex
    ( get )
    where

import Data.List qualified as List
import Data.Text qualified as Text
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Parsers.JSON.Istex (toDoc)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude hiding (get)
import ISTEX qualified as ISTEX
import ISTEX.Client qualified as ISTEX

type Query = Text
type MaxResults = Maybe Int

get :: Lang -> Query -> MaxResults -> IO [HyperdataDocument]
get la query' maxResults = do
  --printDebug "[Istex.get] calling getMetadataScrollProgress for la" la
  --printDebug "[Istex.get] calling getMetadataScrollProgress for q" q
  --printDebug "[Istex.get] calling getMetadataScrollProgress for ml" ml
  -- The "scroll" expects "d/h/m/s/ms" time interval. Let's set it to "1 month"
  --eDocs <- ISTEX.getMetadataScroll q ((\_n -> pack $ "1m") <$> ml) Nothing 0  --(fromIntegral <$> ml)

  -- TODO check if abstract is in query already if not add like below
  -- eDocs <- ISTEX.getMetadataScroll (q <> " abstract:*")  "1m" Nothing 0  --(fromIntegral <$> ml)
  -- eDocs <- ISTEX.getMetadataScroll q "1m" Nothing 0  --(fromIntegral <$> ml)

  let query = case (List.length $ Text.splitOn ":" query') == 1 of
        -- True case means users is entering default search of IsTex
        -- In that case we need to enrich his query with 2 parameters
        -- First expected language: user has to define it in GTXT
        -- Second : query in abstract
        True  -> ("language:"<> toISTEXLanguageCode la) <> " AND abstract:"<>query'

        False -> query'
        -- Complex queries of IsTex needs parameters using ":" so we leave the query as it is
        -- in that case we suppose user is knowing what s.he is doing

  eDocs <- ISTEX.getMetadataWith query maxResults
  -- printDebug "[Istex.get] will print length" (0 :: Int)
  case eDocs of
    Left _ -> pure ()
    Right (ISTEX.Documents { _documents_hits }) -> printDebug "[Istex.get] length docs" $ length _documents_hits
  --ISTEX.getMetadataScrollProgress q ((\_ -> pack $ "1m") <$> ml) Nothing progress errorHandler
  case eDocs of
    Left err -> panicTrace . Text.pack . show $ err
    Right docs -> toDoc' la docs
  --pure $ either (panic . pack . show) (toDoc' la) eDocs
--  where
--    progress (ISTEX.ScrollResponse { _scroll_documents = ISTEX.Documents { _documents_hits }}) =
--      printDebug "[Istex.get] got docs: " $ length _documents_hits
--    errorHandler err = printDebug "[Istex.get] error" $ show err

toDoc' :: Lang -> ISTEX.Documents -> IO [HyperdataDocument]
toDoc' la docs' =  mapM (toDoc la) (ISTEX._documents_hits docs')
  --printDebug "ISTEX" (ISTEX._documents_total docs')

-- | Returns the properly-rendered language code according to
-- https://doc.istex.fr/tdm/annexes/codes-langues.html
toISTEXLanguageCode :: Lang -> Text.Text
toISTEXLanguageCode = \case
  DE -> "ger"
  EL -> "gre"
  EN -> "eng"
  ES -> "spa"
  FR -> "fre"
  IT -> "ita"
  PL -> "pol"
  PT -> "por"
  RU -> "Rus"
  UK -> "ukr"
  ZH -> "chi"
