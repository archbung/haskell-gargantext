{-|
Module      : Gargantext.Core.Text.Corpus.API.OpenAlex
Description : OpenAlex API connection
Copyright   : (c) CNRS, 2023
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}
module Gargantext.Core.Text.Corpus.API.OpenAlex where

import Conduit
import Data.LanguageCodes qualified as ISO639
import qualified Data.Text as T
import Gargantext.Core (iso639ToText)
import Gargantext.Core.Text.Corpus.Query as Corpus
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Protolude
import qualified OpenAlex as OA
import qualified OpenAlex.Types as OA
import Servant.Client (ClientError)


get :: Text
    -> Corpus.RawQuery
    -> Maybe ISO639.ISO639_1
    -> Maybe Limit
    -> IO (Either ClientError (Maybe Integer, ConduitT () HyperdataDocument IO ()))
get _email q lang mLimit = do
  let limit = getLimit $ fromMaybe 1000 mLimit
  let mFilter = (\l -> "language:" <> iso639ToText l) <$> lang
  eRes <- OA.fetchWorksC Nothing mFilter $ Just $ Corpus.getRawQuery q
  pure $ (\(len, docsC) -> (len, docsC .| takeC limit .| mapC toDoc)) <$> eRes

toDoc :: OA.Work -> HyperdataDocument
toDoc (OA.Work { .. } ) =
  HyperdataDocument { _hd_bdd = Just "OpenAlex"
                    , _hd_doi = doi
                    , _hd_url = url
                    , _hd_uniqId = Just id
                    , _hd_uniqIdBdd = Just id
                    , _hd_page = firstPage biblio
                    , _hd_title = title
                    , _hd_authors = authors authorships
                    , _hd_institutes = institutes authorships
                    , _hd_source = source
                    , _hd_abstract = Just abstract_reconstructed
                    , _hd_publication_date = Just $ show publication_date
                    , _hd_publication_year = Just $ publication_year
                    , _hd_publication_month = Nothing  -- TODO
                    , _hd_publication_day = Nothing  -- TODO
                    , _hd_publication_hour = Nothing  -- TODO
                    , _hd_publication_minute = Nothing  -- TODO
                    , _hd_publication_second = Nothing  -- TODO
                    , _hd_language_iso2 = language }
      where
        firstPage :: OA.Biblio -> Maybe Int
        firstPage OA.Biblio { first_page } = maybe Nothing readMaybe $ T.unpack <$> first_page

        authors :: [OA.Authorship] -> Maybe Text
        authors [] = Nothing
        authors aus = Just $ T.intercalate ", " $ catMaybes (getDisplayName <$> aus)
          where
            getDisplayName :: OA.Authorship -> Maybe Text
            getDisplayName OA.Authorship { author = OA.DehydratedAuthor { display_name = dn } } = dn

        institutes :: [OA.Authorship] -> Maybe Text
        institutes [] = Nothing
        institutes aus = Just $ T.intercalate ", " ((T.replace ", " " - ") . getInstitutesNames <$> aus)
          where
            getInstitutesNames OA.Authorship { institutions } = T.intercalate ", " $ getDisplayName <$> institutions
            getDisplayName :: OA.DehydratedInstitution -> Text
            getDisplayName OA.DehydratedInstitution { display_name = dn } = dn

        source :: Maybe Text
        source = maybe Nothing getSource primary_location
          where
            getSource OA.Location { source = s } = getSourceDisplayName <$> s
            getSourceDisplayName OA.DehydratedSource { display_name = dn }  = dn
