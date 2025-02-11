{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.Wikidata
<<<<<<< HEAD
Description : To query Wikidata
=======
Description : To query Wikidata
>>>>>>> dev-clustering
Copyright   : (c) CNRS, 2019-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.Core.Text.Corpus.Parsers.Wikidata where

import Control.Lens (makeLenses, (^.) )
import Data.List qualified as List
import Data.Text (concat)
import Database.HSparql.Connection ( BindingValue, EndPoint, selectQueryRaw )
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Parsers.Date (mDateSplit)
import Gargantext.Core.Text.Corpus.Parsers.Isidore (unbound)
import Gargantext.Core.Text.Corpus.Parsers.Wikidata.Crawler ( crawlPage )
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Prelude hiding (concat)
import Prelude qualified



data WikiResult = WikiResult { _wr_cid         :: Maybe Text
                             , _wr_title       :: Maybe Text
                             , _wr_url         :: Maybe Text
                             , _wr_yearStart   :: Maybe Text
                             , _wr_yearEnd     :: Maybe Text
                             , _wr_yearFlorish :: Maybe Text
                             } deriving (Show, Eq)
$(makeLenses ''WikiResult)

type NumberOfSections = Int

wikidataGet :: Int -> NumberOfSections -> IO [HyperdataDocument]
wikidataGet n m = do
  results <- wikidataSelect n
  mapM (wikiPageToDocument m) results


wikiPageToDocument :: NumberOfSections -> WikiResult ->  IO HyperdataDocument
wikiPageToDocument m wr = do
  sections <- case wr ^. wr_url of
    Nothing -> pure []
    Just  u -> crawlPage u

  let bdd    = Just "wikidata"
      doi    = Nothing
      url    = wr ^. wr_url
      page      = Nothing
      title     = wr ^. wr_title
      authors    = Nothing
      institutes = Nothing
      source     = Nothing
      abstract   = Just $ concat $ take m sections

  let mDateS = head $ catMaybes
               [ wr ^. wr_yearStart
               , wr ^. wr_yearEnd
               , wr ^. wr_yearFlorish
               , head sections
               ]
  let (date, (year, month, day)) = mDateSplit mDateS

  let hour = Nothing
      minute = Nothing
      sec = Nothing
      iso2   = Just $ show EN

  pure $ HyperdataDocument { _hd_bdd = bdd
                           , _hd_doi = doi
                           , _hd_url = url
                           , _hd_page = page
                           , _hd_title = title
                           , _hd_authors = authors
                           , _hd_institutes = institutes
                           , _hd_source = source
                           , _hd_abstract = abstract
                           , _hd_publication_date = show <$> date
                           , _hd_publication_year = year
                           , _hd_publication_month = month
                           , _hd_publication_day = day
                           , _hd_publication_hour = hour
                           , _hd_publication_minute = minute
                           , _hd_publication_second = sec
                           , _hd_language_iso2 = iso2 }


wikidataSelect :: Int -> IO [WikiResult]
wikidataSelect n = do
  result <- selectQueryRaw wikidataRoute (wikidataQuery n)
  case result of
    Nothing      -> pure []
    Just result' -> pure $ map toWikiResult $ unbound' EN result'


unbound' :: Lang -> [[BindingValue]] -> [[Maybe Text]]
unbound' l = map (map (unbound l))

toWikiResult :: [Maybe Text] -> WikiResult
toWikiResult (c:t:u:ys:ye:yf:_) = WikiResult c t u ys ye yf
toWikiResult _                  = panicTrace "[G.C.T.C.Parsers.Wikidata.toWikiResult] error"

wikidataRoute :: EndPoint
wikidataRoute = "https://query.wikidata.org/sparql"

wikidataQuery :: Int -> Prelude.String
wikidataQuery n = List.unlines
      ["     PREFIX wd:       <http://www.wikidata.org/entity/>"
      ,"     PREFIX wdt:      <http://www.wikidata.org/prop/direct/>"
      ,"     PREFIX schema:   <http://schema.org/>"
      ,"     PREFIX wikibase: <http://wikiba.se/ontology#>"
      ,"     SELECT DISTINCT "
      ,"      ?cid"
      ,"      ?title"
      ,"      ?url"
      ,"      (year(xsd:dateTime(?dateStart))   as ?yearStart)"
      ,"      (year(xsd:dateTime(?dateEnd))     as ?yearEnd)"
      ,"      (year(xsd:dateTime(?dateFlorish)) as ?yearFlorish) "
      ,"     WHERE {"
      ,"       ?cid wdt:P31 wd:Q968159 ."
      ,"       ?cid rdfs:label ?title filter (lang(?title) = \"en\") ."
      ,"      "
      ,"       ?url schema:about ?cid ."
      ,"       ?url schema:inLanguage \"en\" ."
      ,"       FILTER (SUBSTR(str(?url), 1, 25) = \"https://en.wikipedia.org/\")"
      ,"       OPTIONAL {?cid (wdt:P580) ?dateStart   .}"
      ,"       OPTIONAL {?cid (wdt:P582) ?dateEnd     .}"
      ,"       OPTIONAL {?cid (wdt:P571) ?dateFlorish .}"
      ,"     }"
      ,"       LIMIT " <> show n
      ]
