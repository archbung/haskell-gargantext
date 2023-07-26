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

import Conduit
import Data.Either
import Data.LanguageCodes qualified as ISO639
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text, pack, intercalate)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Parsers.Date qualified as Date
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Defaults qualified as Defaults
import Gargantext.Prelude
import HAL qualified as HAL
import HAL.Client qualified as HAL
import HAL.Doc.Corpus qualified as HAL
import Servant.Client (ClientError)

toLang :: Lang -> Maybe ISO639.ISO639_1
toLang DE = Just ISO639.DE
toLang EL = Just ISO639.EL
toLang EN = Just ISO639.EN
toLang ES = Just ISO639.ES
toLang FR = Just ISO639.FR
toLang IT = Just ISO639.IT
toLang PL = Just ISO639.PL
toLang PT = Just ISO639.PT
toLang RU = Just ISO639.RU
toLang UK = Just ISO639.UK
toLang ZH = Just ISO639.ZH
toLang All = Nothing

get :: Lang -> Text -> Maybe Int -> IO [HyperdataDocument]
get la q ml = do
  eDocs <- HAL.getMetadataWith [q] (Just 0) (fromIntegral <$> ml) (toLang la)
  either (panic . pack . show) (\d -> mapM (toDoc' la) $ HAL._docs d) eDocs

getC :: Lang -> Text -> Maybe Int -> IO (Either ClientError (Maybe Integer, ConduitT () HyperdataDocument IO ()))
getC la q ml = do
  eRes <- HAL.getMetadataWithC [q] (Just 0) (fromIntegral <$> ml) (toLang la)
  pure $ (\(len, docsC) -> (len, docsC .| mapMC (toDoc' la))) <$> eRes
--  case eRes of
--    Left err -> panic $ pack $ show err
--    Right (len, docsC) -> pure (len, docsC .| mapMC (toDoc' la))

toDoc' :: Lang -> HAL.Corpus -> IO HyperdataDocument
toDoc' la h@(HAL.Corpus { .. }) = do
  -- printDebug "[toDoc corpus] h" h
  (utctime, (pub_year, pub_month, pub_day)) <-
        Date.dateSplit la (maybe (Just $ pack $ show Defaults.year) Just _corpus_date)
  let abstractDefault = intercalate " " _corpus_abstract
  let abstract = case toLang la of
        Nothing -> abstractDefault
        Just l  -> fromMaybe abstractDefault (intercalate " " <$> Map.lookup l _corpus_abstract_lang_map)
  pure HyperdataDocument { _hd_bdd = Just "Hal"
                         , _hd_doi = Just $ pack $ show _corpus_docid
                         , _hd_url = Nothing
                         , _hd_uniqId = Nothing
                         , _hd_uniqIdBdd = Nothing
                         , _hd_page = Nothing
                         , _hd_title = Just $ intercalate " " _corpus_title
                         , _hd_authors = Just $ foldl (\x y -> x <> ", " <> y) "" _corpus_authors_names
                         , _hd_institutes = Just $ foldl (\x y -> x <> ", " <> y) "" $ _corpus_authors_affiliations <> map (cs . show) _corpus_struct_id
                         , _hd_source = Just $ maybe "Nothing" identity _corpus_source
                         , _hd_abstract = Just abstract
                         , _hd_publication_date = fmap (pack . show) utctime
                         , _hd_publication_year = pub_year
                         , _hd_publication_month = pub_month
                         , _hd_publication_day = pub_day
                         , _hd_publication_hour = Nothing
                         , _hd_publication_minute = Nothing
                         , _hd_publication_second = Nothing
                         , _hd_language_iso2 = Just $ (pack . show) la }
