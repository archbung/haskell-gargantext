{-|
Module      : Gargantext.API.GraphQL.Context
Description :
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.Context where

-- TODO Add support for adding FrameWrite comments for a Context

import Data.Morpheus.Types
  ( GQLType
  , Resolver
  , ResolverM
  , QUERY
  )
import Data.Text (pack)
import Data.Time.Format.ISO8601 (iso8601Show)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Errors.Types ( BackendInternalError )
import Gargantext.API.Prelude (GargM)
import Gargantext.Core.Types.Search (HyperdataRow(..), toHyperdataRow)
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument )
import Gargantext.Database.Admin.Types.Node (ContextTitle, NodeId(..), NodeTypeId, UserId, unNodeId, ContextId (..))
import Gargantext.Database.Prelude (CmdCommon)
import Gargantext.Database.Query.Table.NodeContext (getNodeContext, getContextsForNgramsTerms, ContextForNgramsTerms(..), {- getContextNgrams, -} getContextNgramsMatchingFTS)
import Gargantext.Database.Query.Table.NodeContext qualified as DNC
import Gargantext.Database.Schema.NodeContext (NodeContext, NodeContextPoly(..))
import Gargantext.Prelude
import Gargantext.Prelude.Crypto.Hash (Hash)

data ContextGQL = ContextGQL
  { c_id        :: Int
  , c_hash_id   :: Maybe Hash
  , c_typename  :: NodeTypeId
  , c_user_id   :: UserId
  , c_parent_id :: Maybe Int
  , c_name      :: ContextTitle
  , c_date      :: Text  -- TODO UTCTime
  , c_hyperdata :: Maybe HyperdataRowDocumentGQL
  , c_score     :: Maybe Double
  , c_category  :: Maybe Int
  } deriving (Generic, GQLType, Show)

-- We need this type instead of HyperdataRow(HyperdataRowDocument)
-- because the latter is a sum type (of doc and contact) and we return
-- docs here only. Without the union type, GraphQL endpoint is simpler.
data HyperdataRowDocumentGQL =
  HyperdataRowDocumentGQL { hrd_abstract           :: Text
                          , hrd_authors            :: Text
                          , hrd_bdd                :: Text
                          , hrd_doi                :: Text
                          , hrd_institutes         :: Text
                          , hrd_language_iso2      :: Text
                          , hrd_page               :: Int
                          , hrd_publication_date   :: Text
                          , hrd_publication_day    :: Int
                          , hrd_publication_hour   :: Int
                          , hrd_publication_minute :: Int
                          , hrd_publication_month  :: Int
                          , hrd_publication_second :: Int
                          , hrd_publication_year   :: Int
                          , hrd_source             :: Text
                          , hrd_title              :: Text
                          , hrd_url                :: Text
                          } deriving (Generic, GQLType, Show)

data NodeContextGQL = NodeContextGQL
   { nc_id         :: Maybe Int
   , nc_node_id    :: Int
   , nc_context_id :: Int
   , nc_score      :: Maybe Double
   , nc_category   :: Maybe Int
   } deriving (Generic, GQLType, Show)

-- | Arguments to the "context node" query.
--   "context_id" is doc id
--   "node_id" is it's corpus id
data NodeContextArgs
  = NodeContextArgs
    { context_id :: Int
    , node_id    :: Int
    } deriving (Generic, GQLType)

data ContextsForNgramsArgs
  = ContextsForNgramsArgs
    { corpus_id    :: Int
    , ngrams_terms :: [Text]
    } deriving (Generic, GQLType)

data NodeContextCategoryMArgs = NodeContextCategoryMArgs
  { context_id :: Int
  , node_id    :: Int
  , category   :: Int
  } deriving (Generic, GQLType)

data ContextNgramsArgs
  = ContextNgramsArgs
    { context_id  :: Int
    , list_id     :: Int }
    deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env BackendInternalError)
type GqlM' e env a = ResolverM e (GargM env BackendInternalError) a

-- GQL API

-- | Function to resolve context from a query.
resolveNodeContext
  :: (CmdCommon env)
  => NodeContextArgs -> GqlM e env [NodeContextGQL]
resolveNodeContext NodeContextArgs { context_id, node_id } =
  dbNodeContext context_id node_id

resolveContextsForNgrams
  :: (CmdCommon env)
  => ContextsForNgramsArgs -> GqlM e env [ContextGQL]
resolveContextsForNgrams ContextsForNgramsArgs { corpus_id, ngrams_terms } =
  dbContextForNgrams corpus_id ngrams_terms

resolveContextNgrams
  :: (CmdCommon env)
  => ContextNgramsArgs -> GqlM e env [Text]
resolveContextNgrams ContextNgramsArgs { context_id, list_id } =
  dbContextNgrams context_id list_id

-- DB

-- | Inner function to fetch the node context DB.
dbNodeContext
  :: (CmdCommon env)
  => Int -> Int -> GqlM e env [NodeContextGQL]
dbNodeContext context_id node_id = do
  -- lift $ printDebug "[dbUsers]" user_id
--  user <- getUsersWithId user_id
--  hyperdata <- getUserHyperdata user_id
--  lift (map toUser <$> zip user hyperdata)
  c <- lift $ getNodeContext (UnsafeMkContextId context_id) (UnsafeMkNodeId node_id)
  pure $ toNodeContextGQL <$> [c]

-- | Returns list of `ContextGQL` for given ngrams in given corpus id.
dbContextForNgrams
  :: (CmdCommon env)
  => Int -> [Text] -> GqlM e env [ContextGQL]
dbContextForNgrams node_id ngrams_terms = do
  contextsForNgramsTerms <- lift $ getContextsForNgramsTerms (UnsafeMkNodeId node_id) ngrams_terms
  --lift $ printDebug "[dbContextForNgrams] contextsForNgramsTerms" contextsForNgramsTerms
  pure $ toContextGQL <$> contextsForNgramsTerms

-- | Fetch ngrams matching given context in a given list id.
dbContextNgrams
  :: (CmdCommon env)
  => Int -> Int -> GqlM e env [Text]
dbContextNgrams context_id list_id = do
  lift $ getContextNgramsMatchingFTS (UnsafeMkContextId context_id) (UnsafeMkNodeId list_id)

-- Conversion functions

toNodeContextGQL :: NodeContext -> NodeContextGQL
toNodeContextGQL (NodeContext { _nc_node_id = UnsafeMkNodeId nc_node_id
                              , _nc_context_id = UnsafeMkNodeId nc_context_id
                              , .. }) =
  NodeContextGQL { nc_id = _nc_id
                 , nc_node_id
                 , nc_context_id
                 , nc_score = _nc_score
                 , nc_category = _nc_category }

toContextGQL :: ContextForNgramsTerms -> ContextGQL
toContextGQL ContextForNgramsTerms { _cfnt_nodeId = c_id
                                   , _cfnt_hash = c_hash_id
                                   , _cfnt_nodeTypeId = c_typename
                                   , _cfnt_userId = c_user_id
                                   , _cfnt_parentId = m_c_parent_id
                                   , _cfnt_c_title = c_name
                                   , _cfnt_date = c_date
                                   , _cfnt_hyperdata =hyperdata
                                   , _cfnt_score = c_score
                                   , _cfnt_category = c_category } =
  ContextGQL { c_id = unNodeId c_id
             , c_parent_id = unNodeId <$> m_c_parent_id
             , c_date = pack $ iso8601Show c_date
             , c_hyperdata = toHyperdataRowDocumentGQL hyperdata
             , c_score
             , c_category
             , .. }

toHyperdataRowDocumentGQL :: HyperdataDocument -> Maybe HyperdataRowDocumentGQL
toHyperdataRowDocumentGQL hyperdata =
  case toHyperdataRow hyperdata of
    HyperdataRowDocument { .. } ->
      Just $ HyperdataRowDocumentGQL { hrd_abstract           = _hr_abstract
                                     , hrd_authors            = _hr_authors
                                     , hrd_bdd                = _hr_bdd
                                     , hrd_doi                = _hr_doi
                                     , hrd_institutes         = _hr_institutes
                                     , hrd_language_iso2      = _hr_language_iso2
                                     , hrd_page               = _hr_page
                                     , hrd_publication_date   = _hr_publication_date
                                     , hrd_publication_day    = _hr_publication_day
                                     , hrd_publication_hour   = _hr_publication_hour
                                     , hrd_publication_minute = _hr_publication_minute
                                     , hrd_publication_month  = _hr_publication_month
                                     , hrd_publication_second = _hr_publication_second
                                     , hrd_publication_year   = _hr_publication_year
                                     , hrd_source             = _hr_source
                                     , hrd_title              = _hr_title
                                     , hrd_url                = _hr_url
                                     }
    HyperdataRowContact { } -> Nothing

updateNodeContextCategory :: (CmdCommon env, HasSettings env)
                          => NodeContextCategoryMArgs -> GqlM' e env [Int]
updateNodeContextCategory NodeContextCategoryMArgs { context_id, node_id, category } = do
  _ <- lift $ DNC.updateNodeContextCategory (UnsafeMkContextId context_id) (UnsafeMkNodeId node_id) category

  pure [1]
