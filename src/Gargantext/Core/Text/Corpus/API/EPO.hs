{-|
Module      : Gargantext.Core.Text.Corpus.API.EPO
Description : EPO (patents) API interface
Copyright   : (c) CNRS, 2023
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}
module Gargantext.Core.Text.Corpus.API.EPO where

import Conduit ( ConduitT, (.|), mapC )
import Data.LanguageCodes (ISO639_1)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import EPO.API.Client.Types qualified as EPO
import EPO.API.Client.Implementation qualified as EPO
import Gargantext.Core (iso639ToText)
import Gargantext.Core.Text.Corpus.Query qualified as Corpus
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument(..) )
import Network.URI (parseURI)
import Protolude
import Servant.Client.Core (ClientError(ConnectionError))


get :: Maybe EPO.AuthKey
    -> Text
    -> Corpus.RawQuery
    -> ISO639_1
    -> Maybe Corpus.Limit
    -> IO (Either ClientError (Maybe Integer, ConduitT () HyperdataDocument IO ()))
get Nothing _ _ _ _ = do
  -- throwIO $ EPO.OtherError "AuthKey is required"
  pure $ Left $ ConnectionError $ toException $ ErrorCall "AuthKey is required"
get (Just authKey) epoAPIUrl q lang mLimit = do
  let limit = Corpus.getLimit <$> mLimit
  case parseURI (T.unpack epoAPIUrl) of
    Nothing -> pure $ Left $ ConnectionError $ toException $ ErrorCall "Cannot parse API URL"
    Just apiUrl -> do
      eRes <- EPO.searchEPOAPIC apiUrl authKey Nothing limit (Corpus.getRawQuery q)
      pure $ (\(total, itemsC) -> (Just total, itemsC .| mapC (toDoc lang))) <$> eRes

      -- EPO.Paginated { .. } <- EPO.searchEPOAPI apiUrl authKey 1 20 (Corpus.getRawQuery q)
      -- pure $ Right ( Just $ fromIntegral total, yieldMany items .| mapC (toDoc lang) )

toDoc :: ISO639_1 -> EPO.HyperdataDocument -> HyperdataDocument
toDoc lang (EPO.HyperdataDocument { .. }) =
  HyperdataDocument { _hd_bdd = Just "EPO"
                    , _hd_doi = Nothing
                    , _hd_url = Nothing
                    , _hd_page = Nothing
                    , _hd_title = Map.lookup lang titles
                    , _hd_authors = authors_
                    , _hd_institutes = Nothing
                    , _hd_source = Nothing
                    , _hd_abstract = Map.lookup lang abstracts
                    , _hd_publication_date = publication_date
                    , _hd_publication_year = publication_year
                    , _hd_publication_month = publication_month
                    , _hd_publication_day = publication_day
                    , _hd_publication_hour = Nothing
                    , _hd_publication_minute = Nothing
                    , _hd_publication_second = Nothing
                    , _hd_language_iso2 = Just $ iso639ToText lang }

  where
    authors_ = if null authors
      then Nothing
      else Just (T.intercalate ", " authors)

--   EPO.withAuthKey authKey $ \token -> do
--     let range = EPO.Range { rBegin = 1, rEnd = limit }
--     (len, docsC) <- EPO.searchPublishedDataWithFetchC token (Just $ Corpus.getRawQuery q) (Just range)
--     pure (len, docsC .|
--                takeC limit .|
--                mapC (toDoc lang))

-- toDoc :: ISO639_1 -> EPO.ExchangeDocument -> HyperdataDocument
-- toDoc lang (EPO.ExchangeDocument { bibliographicData = EPO.BibliographicData { .. }
--                                  , abstracts } ) =
--   HyperdataDocument { _hd_bdd = Just "EPO"
--                     , _hd_doi = Nothing
--                     , _hd_url = Nothing
--                     , _hd_uniqId = EPO.documentIdToText <$> (head documentIds)
--                     , _hd_uniqIdBdd = EPO.documentIdToText <$> (head documentIds)
--                     , _hd_page = Nothing
--                     , _hd_title = Map.lookup lang inventionTitlesMap
--                     , _hd_authors = authors parties
--                     , _hd_institutes = Nothing
--                     , _hd_source = Nothing
--                     , _hd_abstract = Map.lookup lang abstractMap
--                     , _hd_publication_date = T.pack <$> showGregorian <$> publicationDate
--                     , _hd_publication_year = year
--                     , _hd_publication_month = month
--                     , _hd_publication_day = day
--                     , _hd_publication_hour = Nothing
--                     , _hd_publication_minute = Nothing
--                     , _hd_publication_second = Nothing
--                     , _hd_language_iso2 = Just $ iso639ToText lang }
--       where
--         authors :: EPO.Parties -> Maybe Text
--         authors (EPO.Parties { inventors = Nothing }) = Nothing
--         authors (EPO.Parties { inventors = Just EPO.Inventors { inventors } }) =
--           Just $ T.intercalate ", " (getInventorName <$> inventors)

--         getInventorName :: EPO.Inventor -> Text
--         getInventorName (EPO.Inventor { inventorName = EPO.InventorName { name } }) = name

--         abstractMap :: Map.Map ISO639_1 Text
--         abstractMap = Map.fromList [(l, text) | (EPO.Abstract { lang = l, text }) <- abstracts]

--         EPO.PublicationReferenceDet { documentIds } = publicationReference

--         dates :: [Day]
--         dates = catMaybes (EPO.date <$> documentIds)

--         publicationDate :: Maybe Day
--         publicationDate = head dates

--         (year, month, day) = case publicationDate of
--           Nothing -> (Nothing, Nothing, Nothing)
--           Just pd -> let (y, m, d) = toGregorian pd in
--             (Just $ fromIntegral y, Just m, Just d)

--         inventionTitlesMap :: Map ISO639_1 Text
--         inventionTitlesMap = Map.fromList [(l, text) | EPO.InventionTitle { lang = l, text } <- inventionTitles]
