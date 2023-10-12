{-|
Module      : Gargantext.Core.Text.Corpus.API.Pubmed
Description : Pubmed API connection
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DerivingStrategies #-}

module Gargantext.Core.Text.Corpus.API.Pubmed
    ( get
    -- * Internals for testing
    , ESearch(..)
    , convertQuery
    , getESearch
    )
    where

import Conduit
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Query as Corpus
import Gargantext.Core.Types (Term(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude hiding (get)
import Network.HTTP.Types.URI (EscapeItem(..), renderQueryPartialEscape)
import PUBMED qualified as PubMed
import PUBMED.Parser qualified as PubMedDoc
import PUBMED.Types (Config(..))
import Servant.Client (ClientError)


-- | A pubmed query.
-- See: https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch
-- The documentation for PUBMED says:
-- Values for query keys may also be provided in term if they are preceeded by a
-- '#' (%23 in the URL). While only one query_key parameter can be provided to ESearch,
-- any number of query keys can be combined in term. Also, if query keys are provided in term,
-- they can be combined with OR or NOT in addition to AND.
-- Example:
-- esearch.fcgi?db=pubmed&term=%231+AND+asthma&WebEnv=<webenv string>&usehistory=y
--
-- Therefore, we can pretty-print our 'Query' back into something that PubMed could understand.
newtype ESearch = ESearch { _ESearch :: [EscapeItem] }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | Returns an /url encoded/ query ready to be sent to pubmed.
getESearch :: ESearch -> Text
getESearch (ESearch items) =
  Text.replace "term=" "" . TE.decodeUtf8 . renderQueryPartialEscape False $ [
    ("term", items)
    ]

convertQuery :: Corpus.Query -> ESearch
convertQuery q = ESearch (interpretQuery q transformAST)
  where
    transformAST :: BoolExpr Term -> [EscapeItem]
    transformAST ast = case ast of
      BAnd sub (BConst (Negative term))
        -- The second term become positive, so that it can be translated.
        -> (transformAST sub) <> [QN "+AND+NOT+"] <> transformAST (BConst (Positive term))
      BAnd term1 (BNot term2)
        -> transformAST term1 <> [QN "+AND+NOT+"] <> transformAST term2
      BAnd sub1 sub2
        -> transformAST sub1 <> [QN "+AND+"] <> transformAST sub2
      BOr sub1 sub2
        -> transformAST sub1 <> [QN "+OR+"] <> transformAST sub2
      BNot (BConst (Negative term))
        -> transformAST (BConst (Positive term)) -- double negation
      BNot sub
        -> [QN "NOT+"] <> transformAST sub
      -- BTrue cannot happen is the query parser doesn't support parsing 'TRUE' alone.
      BTrue
        -> mempty
      -- BTrue cannot happen is the query parser doesn't support parsing 'FALSE' alone.
      BFalse
        -> mempty
      BConst (Positive (Term term))
        -> [QE (TE.encodeUtf8 term)]
      -- We can handle negatives via `ANDNOT` with itself.
      BConst (Negative (Term term))
        -> [QN "NOT+", QE (TE.encodeUtf8 term)]

get :: Text
    -> Corpus.RawQuery
    -> Maybe Limit
    -> IO (Either ClientError (Maybe Integer, ConduitT () HyperdataDocument IO ()))
get apiKey q l = do
  -- NOTE(adinapoli): For now we do not interpret the PUBMED query into something
  -- more structured, like an 'ESearch' term, but we could, in the future.
  eRes <- runReaderT PubMed.getMetadataWithC (Config { apiKey  = Just apiKey
                                                     , query   = getRawQuery q
                                                     , perPage = Just 200
                                                     , mWebEnv = Nothing })
  let takeLimit = case l of
        Nothing -> mapC identity
        Just l' -> takeC $ getLimit l'
  pure $ (\(len, docsC) -> (len, docsC .| takeLimit .| mapC (toDoc EN))) <$> eRes
  --either (\e -> panic $ "CRAWL: PubMed" <> e) (map (toDoc EN))
  --      <$> PubMed.getMetadataWithC q l

toDoc :: Lang -> PubMedDoc.PubMed -> HyperdataDocument
toDoc l (PubMedDoc.PubMed { pubmed_id
                          , pubmed_article = PubMedDoc.PubMedArticle t j as aus
                          , pubmed_date = PubMedDoc.PubMedDate a y m d }
          ) = HyperdataDocument { _hd_bdd = Just "PubMed"
                                , _hd_doi = Nothing
                                , _hd_url = Nothing
                                , _hd_uniqId = Just $ Text.pack $ show pubmed_id
                                , _hd_uniqIdBdd = Nothing
                                , _hd_page = Nothing
                                , _hd_title = t
                                , _hd_authors = authors aus
                                , _hd_institutes = institutes aus
                                , _hd_source = j
                                , _hd_abstract = abstract as
                                , _hd_publication_date = Just $ Text.pack $ show a
                                , _hd_publication_year = Just $ fromIntegral y
                                , _hd_publication_month = Just m
                                , _hd_publication_day = Just d
                                , _hd_publication_hour = Nothing
                                , _hd_publication_minute = Nothing
                                , _hd_publication_second = Nothing
                                , _hd_language_iso2 = Just $ (Text.pack . show) l }
      where
        authors :: [PubMedDoc.Author] -> Maybe Text
        authors [] = Nothing
        authors au = Just $ (Text.intercalate ", ")
                          $ catMaybes
                          $ map (\n -> PubMedDoc.foreName n <> Just " " <> PubMedDoc.lastName n) au

        institutes :: [PubMedDoc.Author] -> Maybe Text
        institutes [] = Nothing
        institutes au = Just $ (Text.intercalate ", ")
                             $ (map (Text.replace ", " " - "))
                             $ catMaybes
                             $ map PubMedDoc.affiliation au


        abstract :: [Text] -> Maybe Text
        abstract [] = Nothing
        abstract as' = Just $ Text.intercalate ", " as'
