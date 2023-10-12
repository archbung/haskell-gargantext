{-|
Module      : Gargantext.API.Node
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-- TODO-ACCESS: CanGetNode
-- TODO-EVENTS: No events as this is a read only query.
Node API

-------------------------------------------------------------------
-- TODO-ACCESS: access by admin only.
--              At first let's just have an isAdmin check.
--              Later: check userId CanDeleteNodes Nothing
-- TODO-EVENTS: DeletedNodes [NodeId]
--              {"tag": "DeletedNodes", "nodes": [Int*]}


-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}

module Gargantext.API.Table
  where

import Data.Aeson.TH (deriveJSON)
import Data.Maybe
import Data.Swagger
import Data.Text qualified as T
import Gargantext.API.HashedResponse
import Gargantext.API.Ngrams.Types (TabType(..))
import Gargantext.API.Prelude (GargServer)
import Gargantext.Core.Text.Corpus.Query (RawQuery, parseQuery, getRawQuery)
import Gargantext.Core.Types (TableResult(..))
import Gargantext.Core.Types.Query (Offset, Limit)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Action.Learn (FavOrTrash(..), moreLike)
import Gargantext.Database.Action.Search
import Gargantext.Database.Admin.Types.Node hiding (ERROR, DEBUG)
import Gargantext.Database.Prelude (CmdM, DbCmd', DBCmd)
import Gargantext.Database.Query.Facet (FacetDoc , runViewDocuments, runCountDocuments, OrderBy(..), runViewAuthorsDoc)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Prelude
import Gargantext.System.Logging
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

------------------------------------------------------------------------

type TableApi = Summary "Table API"
              :> QueryParam "tabType" TabType
              :> QueryParam "limit" Limit
              :> QueryParam "offset" Offset
              :> QueryParam "orderBy" OrderBy
              :> QueryParam "query" RawQuery
              :> QueryParam "year" Text
              :> Get    '[JSON] (HashedResponse FacetTableResult)
            :<|> Summary "Table API (POST)"
              :> ReqBody '[JSON] TableQuery
              :> Post    '[JSON] FacetTableResult
            :<|> "hash" :>
                   Summary "Hash Table"
                :> QueryParam "tabType" TabType
                :> Get '[JSON] Text

data TableQuery = TableQuery
  { tq_offset       :: Offset
  , tq_limit        :: Limit
  , tq_orderBy      :: OrderBy
  , tq_view         :: TabType
  , tq_query        :: RawQuery
  } deriving (Generic)

type FacetTableResult = TableResult FacetDoc

$(deriveJSON (unPrefix "tq_") ''TableQuery)

instance ToSchema TableQuery where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "tq_")

instance Arbitrary TableQuery where
  arbitrary = elements [TableQuery { tq_offset = 0
                                   , tq_limit = 10
                                   , tq_orderBy = DateAsc
                                   , tq_view = Docs
                                   , tq_query = "electrodes" }]


tableApi :: NodeId -> GargServer TableApi
tableApi id' = getTableApi id'
          :<|> postTableApi id'
          :<|> getTableHashApi id'


getTableApi :: (CmdM env err m, HasNodeError err, MonadLogger m)
            => NodeId
            -> Maybe TabType
            -> Maybe Limit
            -> Maybe Offset
            -> Maybe OrderBy
            -> Maybe RawQuery
            -> Maybe Text
            -> m (HashedResponse FacetTableResult)
getTableApi cId tabType mLimit mOffset mOrderBy mQuery mYear =
  case mQuery of
    Nothing -> get_table
    Just "" -> get_table
    Just q  -> case tabType of
      Just Docs
        -> do
          $(logLocM) DEBUG $ "New search with query " <> getRawQuery q
          constructHashedResponse <$> searchInCorpus' cId False q mOffset mLimit mOrderBy
      Just Trash
        -> constructHashedResponse <$> searchInCorpus' cId True q mOffset mLimit mOrderBy
      _ -> get_table

  where
    get_table = do
      $(logLocM) DEBUG $ "getTable cId = " <> T.pack (show cId)
      t <- getTable cId tabType mOffset mLimit mOrderBy mQuery mYear
      pure $ constructHashedResponse t

postTableApi :: (CmdM env err m, MonadLogger m, HasNodeError err)
             => NodeId
             -> TableQuery
             -> m FacetTableResult
postTableApi cId tq = case tq of
 TableQuery o l order ft "" -> do
   $(logLocM) DEBUG $ "New search with no query"
   getTable cId (Just ft) (Just o) (Just l) (Just order) Nothing Nothing
 TableQuery o l order ft q  -> case ft of
   Docs  -> do
     $(logLocM) DEBUG $ "New search with query " <> getRawQuery q
     searchInCorpus' cId False q (Just o) (Just l) (Just order)
   Trash -> searchInCorpus' cId True  q (Just o) (Just l) (Just order)
   x     -> panic $ "not implemented in tableApi " <> (show x)

getTableHashApi :: (CmdM env err m, HasNodeError err, MonadLogger m)
                => NodeId
                -> Maybe TabType
                -> m Text
getTableHashApi cId tabType = do
  HashedResponse { hash = h } <- getTableApi cId tabType Nothing Nothing Nothing Nothing Nothing
  pure h

searchInCorpus' :: (DbCmd' env err m, MonadLogger m)
                => CorpusId
                -> Bool
                -> RawQuery
                -> Maybe Offset
                -> Maybe Limit
                -> Maybe OrderBy
                -> m FacetTableResult
searchInCorpus' cId t q o l order = do
  case parseQuery q of
    -- FIXME(adn) The error handling needs to be monomorphic over GargErr.
    Left noParseErr -> do
      $(logLocM) ERROR $ "Invalid input query " <> (getRawQuery q) <> " , error = " <> (T.pack noParseErr)
      pure $ TableResult 0 []
    Right boolQuery -> do
      docs          <- searchInCorpus cId t boolQuery o l order
      countAllDocs  <- searchCountInCorpus cId t boolQuery
      pure $ TableResult { tr_docs = docs
                         , tr_count = countAllDocs }


getTable :: HasNodeError err
         => NodeId
         -> Maybe TabType
         -> Maybe Offset
         -> Maybe Limit
         -> Maybe OrderBy
         -> Maybe RawQuery
         -> Maybe Text
         -> DBCmd err FacetTableResult
getTable cId ft o l order raw_query year = do
  docs      <- getTable' cId ft o l order query year
  docsCount <- runCountDocuments cId (ft == Just Trash) query year
  pure $ TableResult { tr_docs = docs, tr_count = docsCount }
  where
    query = getRawQuery <$> raw_query

getTable' :: HasNodeError err
          => NodeId
          -> Maybe TabType
          -> Maybe Offset
          -> Maybe Limit
          -> Maybe OrderBy
          -> Maybe Text
          -> Maybe Text
          -> DBCmd err [FacetDoc]
getTable' cId ft o l order query year =
  case ft of
    (Just Docs)      -> runViewDocuments cId False o l order query year
    (Just Trash)     -> runViewDocuments cId True  o l order query year
    (Just MoreFav)   -> moreLike cId o l order IsFav
    (Just MoreTrash) -> moreLike cId o l order IsTrash
    x     -> panic $ "not implemented in getTable: " <> (show x)


getPair :: ContactId -> Maybe TabType
         -> Maybe Offset  -> Maybe Limit
         -> Maybe OrderBy -> DBCmd err [FacetDoc]
getPair cId ft o l order =
  case ft of
    (Just Docs)  -> runViewAuthorsDoc cId False o l order
    (Just Trash) -> runViewAuthorsDoc cId True  o l order
    _     -> panic $ "not implemented: get Pairing" <> (show ft)
