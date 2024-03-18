{-|
Module      : Gargantext.Core.Text.Corpus.API.Hal
Description : Pubmed API connection
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Core.Text.Corpus.API.Hal
    where

import Conduit ( ConduitT, (.|), mapMC )
import Data.LanguageCodes qualified as ISO639
import Data.Map.Strict qualified as Map
import Data.Text (pack)
import Gargantext.Core.Text.Corpus.Parsers.Date qualified as Date
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument(..) )
import Gargantext.Defaults qualified as Defaults
import Gargantext.Prelude hiding (intercalate)
import HAL qualified
import HAL.Doc.Corpus qualified as HAL
import HAL.Types qualified as HAL
import Servant.Client (ClientError)

get :: Maybe ISO639.ISO639_1 -> Text -> Maybe Int -> IO [HyperdataDocument]
get la q ml = do
  eDocs <- HAL.getMetadataWith [q] (Just 0) (fromIntegral <$> ml) la
  either (panicTrace . pack . show) (mapM (toDoc' la) . HAL._docs) eDocs

getC :: Maybe ISO639.ISO639_1 -> Text -> Maybe Int -> IO (Either ClientError (Maybe Integer, ConduitT () HyperdataDocument IO ()))
getC la q ml = do
  eRes <- HAL.getMetadataWithCursorC q (fromIntegral <$> ml) la
  pure $ (\(len, docsC) -> (len, docsC .| mapMC (toDoc' la))) <$> eRes
--  case eRes of
--    Left err -> panic $ pack $ show err
--    Right (len, docsC) -> pure (len, docsC .| mapMC (toDoc' la))

toDoc' :: Maybe ISO639.ISO639_1 -> HAL.Corpus -> IO HyperdataDocument
toDoc' la (HAL.Corpus { .. }) = do
  -- printDebug "[toDoc corpus] h" h
  let mDateS = _corpus_date <|> Just (pack $ show Defaults.year)
  let (utctime, (pub_year, pub_month, pub_day)) = Date.mDateSplit mDateS
  let abstractDefault = unwords _corpus_abstract
  let abstract = case la of
        Nothing -> abstractDefault
        Just l  -> maybe abstractDefault unwords (Map.lookup l _corpus_abstract_lang_map)
  pure HyperdataDocument { _hd_bdd = Just "Hal"
                         , _hd_doi = Just $ pack $ show _corpus_docid
                         , _hd_url = Nothing
                         , _hd_page = Nothing
                         , _hd_title = Just $ unwords _corpus_title
                         , _hd_authors = Just $ foldl' (\x y -> if x == "" then y else x <> ", " <> y) "" _corpus_authors_names
                         , _hd_institutes = Just $ foldl' (\x y -> if x == "" then y else x <> ", " <> y) "" $ _corpus_authors_affiliations <> map show _corpus_struct_id
                         , _hd_source = Just $ maybe "Nothing" identity _corpus_source
                         , _hd_abstract = Just abstract
                         , _hd_publication_date = fmap show utctime
                         , _hd_publication_year = pub_year
                         , _hd_publication_month = pub_month
                         , _hd_publication_day = pub_day
                         , _hd_publication_hour = Nothing
                         , _hd_publication_minute = Nothing
                         , _hd_publication_second = Nothing
                         , _hd_language_iso2 = Just $ show la }
