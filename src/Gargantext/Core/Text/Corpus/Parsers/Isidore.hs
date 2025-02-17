{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.Isidore
Description : To query French Humanities publication database
Copyright   : (c) CNRS, 2019-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TODO:
- put endpoint in configuration file
- more flexible fields of research
- type database name
- use more ontologies to help building corpora
-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.Core.Text.Corpus.Parsers.Isidore where

import Control.Lens ( (^.), (.~) )
import Data.ByteString.Lazy (ByteString)
import Data.RDF ( Node(LNode, UNode), LValue(PlainLL, TypedL, PlainL) )
import Data.Text qualified as T
import Database.HSparql.Connection ( BindingValue(..), EndPoint, structureContent )
import Database.HSparql.QueryGenerator
import Gargantext.Core (Lang)
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument(..) )
import Gargantext.Prelude hiding (ByteString)
import Network.Wreq (getWith, Response, defaults, header, param, responseStatus, responseBody)
import Prelude qualified

route :: EndPoint
route = "https://isidore.science/sparql/"

selectQueryRaw' :: Prelude.String -> Prelude.String -> IO (Response ByteString)
selectQueryRaw' uri q = getWith opts uri
  where
    opts = defaults & header "Accept"     .~ ["application/sparql-results+xml"]
                    & header "User-Agent" .~ ["gargantext-hsparql-client"]
                    & param  "query"      .~ [T.pack q]

isidoreGet :: Lang -> Int -> Text -> IO (Maybe [HyperdataDocument])
isidoreGet la li q = do
  bindingValues <- isidoreGet' li q
  case bindingValues of
    Nothing -> pure Nothing
    Just dv -> pure $ Just $ map (bind2doc la) dv

isidoreGet' :: Int -> Text -> IO (Maybe [[BindingValue]])
isidoreGet' l q = do
  let s = createSelectQuery $ isidoreSelect l q
  putStrLn s
  r <- selectQueryRaw' route s
  putStrLn (show $ r ^. responseStatus :: Text)
  pure $ structureContent $ r ^. responseBody
 -- res <- selectQuery route $ simpleSelect q
 -- pure res

isidoreSelect :: Int -> Text -> Query SelectQuery
isidoreSelect lim q = do
  -- See Predefined Namespace Prefixes:
  -- https://isidore.science/sparql?nsdecl
  isidore <- prefix "isidore" (iriRef "http://isidore.science/class/")
  rdf     <- prefix "rdf"     (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  dcterms <- prefix "dcterms" (iriRef "http://purl.org/dc/terms/")
  dc      <- prefix "dc"      (iriRef "http://purl.org/dc/elements/1.1/")
  --iso     <- prefix "fra"        (iriRef "http://lexvo.org/id/iso639-3/")
  --ore     <- prefix "ore"    (iriRef "http://www.openarchives.org/ore/terms/")
  --bif     <- prefix "bif"    (iriRef "bif:")

  link'    <- var
  title    <- var
  date     <- var
  abstract <- var
  authors  <- var
  source   <- var
  langDoc  <- var
  publisher <- var
  --agg       <- var

  triple_ link' (rdf     .:. "type")     (isidore .:. "Document")
  triple_ link' (dcterms .:. "title")    title
  triple_ link' (dcterms .:. "date")     date
  triple_ link' (dcterms .:. "creator")  authors
  --triple_ link (dcterms .:. "language") langDoc
  triple_ link' (dc      .:. "description") abstract
  --triple_ link (ore .:. "isAggregatedBy") agg
  --triple_ agg (dcterms .:. "title") title

  optional_ $ triple_ link' (dcterms .:. "source")      source
  optional_ $ triple_ link' (dcterms .:. "publisher")   publisher

  -- TODO FIX BUG with (.||.) operator
  --filterExpr_ $ (.||.) (contains title q) (contains abstract q)
  --filterExpr_ (containsWith authors q) -- (contains abstract q)
  --filterExpr_ (containsWith title q) -- (contains abstract q)
  --filterExpr_ $ (.||.) (containsWith title q) (contains abstract q)
  filterExpr_ (containsWith title q)

  -- TODO FIX filter with lang
  --filterExpr_ $ langMatches title (str ("fra" :: Text))
  --filterExpr_ $ (.==.) langDoc (str ("http://lexvo.org/id/iso639-3/fra" :: Text))

  orderNextDesc date
  limit_ lim
  distinct_
  selectVars [link', date, langDoc, authors, source, publisher, title, abstract]

-- | TODO : check if all cases are taken into account
unbound :: Lang -> BindingValue -> Maybe Text
unbound _ Unbound         = Nothing
unbound _ (Bound (UNode x)) = Just x
unbound _ (Bound (LNode (TypedL x _))) = Just x
unbound _ (Bound (LNode (PlainL x)))   = Just x
unbound l (Bound (LNode (PlainLL x l')))   = if l' == T.toLower (show l) then Just x else Nothing
unbound _ _ = Nothing

bind2doc :: Lang -> [BindingValue] -> HyperdataDocument
bind2doc l [ link', date, langDoc, authors, _source, publisher, title, abstract ] =
  HyperdataDocument { _hd_bdd = Just "Isidore"
                    , _hd_doi = Nothing
                    , _hd_url = unbound l link'
                    , _hd_page = Nothing
                    , _hd_title = unbound l title
                    , _hd_authors = unbound l authors
                    , _hd_institutes = Nothing
                    , _hd_source = unbound l publisher
                    , _hd_abstract = unbound l abstract
                    , _hd_publication_date = unbound l date
                    , _hd_publication_year = Nothing
                    , _hd_publication_month = Nothing
                    , _hd_publication_day = Nothing
                    , _hd_publication_hour = Nothing
                    , _hd_publication_minute = Nothing
                    , _hd_publication_second = Nothing
                    , _hd_language_iso2 = unbound l langDoc }

bind2doc _ _  = undefined
