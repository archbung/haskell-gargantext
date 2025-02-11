{-|
Module      : Gargantext.Database.Query.Table.NodeContext
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Query.Table.NodeContext
  ( module Gargantext.Database.Schema.NodeContext
  , queryNodeContextTable
  , selectDocsDates
  , selectDocNodes
  , selectDocs
  , nodeContextsCategory
  , nodeContextsScore
  , getNodeContexts
  , getNodeContext
  , updateNodeContextCategory
  , getContextsForNgrams
  , ContextForNgrams(..)
  , getContextsForNgramsTerms
  , getContextNgrams
  , getContextNgramsMatchingFTS
  , ContextForNgramsTerms(..)
  , insertNodeContext
  , deleteNodeContext
  , selectPublicContexts
  , selectCountDocs
  )
  where

import Control.Arrow (returnA)
import Control.Lens (view, (^.))
import Data.Text (splitOn)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple qualified as PGS (In(..), Query, Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Gargantext.Core ( HasDBid(toDBid) )
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument, hd_publication_date )
import Gargantext.Database.Admin.Types.Hyperdata.Prelude ( Hyperdata )
import Gargantext.Database.Query.Table.Node.Error (HasNodeError, NodeError(..), nodeError)
import Gargantext.Database.Prelude
import Gargantext.Database.Schema.Context
import Gargantext.Database.Schema.Ngrams ()  -- instances
import Gargantext.Database.Schema.Node ( node_id, node_typename, queryNodeTable, NodeRead )
import Gargantext.Database.Schema.NodeContext
import Gargantext.Prelude
import Gargantext.Prelude.Crypto.Hash (Hash)
import Opaleye
import Opaleye qualified as O

queryNodeContextTable :: Select NodeContextRead
queryNodeContextTable = selectTable nodeContextTable

-- | not optimized (get all ngrams without filters)
_nodesContexts :: DBCmd err [NodeContext]
_nodesContexts = runOpaQuery queryNodeContextTable

------------------------------------------------------------------------
-- | Basic NodeContext tools
getNodeContexts :: NodeId -> DBCmd err [NodeContext]
getNodeContexts n = runOpaQuery (selectNodeContexts $ pgNodeId n)
  where
    selectNodeContexts :: Field SqlInt4 -> Select NodeContextRead
    selectNodeContexts n' = proc () -> do
      ns <- queryNodeContextTable -< ()
      restrict -< _nc_node_id ns .== n'
      returnA -< ns


getNodeContext :: HasNodeError err => ContextId -> NodeId -> DBCmd err NodeContext
getNodeContext c n = do
  maybeNodeContext <- headMay <$> runOpaQuery (selectNodeContext (pgContextId c) (pgNodeId n))
  case maybeNodeContext of
    Nothing -> nodeError (NoContextFound c)
    Just  r -> pure r
  where
    selectNodeContext :: Field SqlInt4 -> Field SqlInt4 -> Select NodeContextRead
    selectNodeContext c' n' = proc () -> do
      ns <- queryNodeContextTable -< ()
      restrict -< _nc_context_id ns .== c'
      restrict -< _nc_node_id ns .== n'
      returnA -< ns

updateNodeContextCategory :: ContextId -> NodeId -> Int -> DBCmd err Int64
updateNodeContextCategory cId nId cat = do
  execPGSQuery upScore (cat, cId, nId)
  where
    upScore :: PGS.Query
    upScore = [sql| UPDATE nodes_contexts
                      SET category = ?
                      WHERE context_id = ?
                      AND node_id = ? |]

data ContextForNgrams =
  ContextForNgrams { _cfn_nodeId    :: NodeId
                   , _cfn_hash      :: Maybe Hash
                   , _cfn_userId    :: UserId
                   , _cfn_parentId  :: Maybe ParentId
                   , _cfn_c_title   :: ContextTitle
                   , _cfn_date      :: UTCTime
                   , _cfn_hyperdata :: HyperdataDocument }
getContextsForNgrams :: HasNodeError err
                     => NodeId
                     -> [Int]
                     -> DBCmd err [ContextForNgrams]
getContextsForNgrams cId ngramsIds = do
  res <- runPGSQuery query (cId, PGS.In ngramsIds)
  pure $ (\( _cfn_nodeId
           , _cfn_hash
           , _cfn_userId
           , _cfn_parentId
           , _cfn_c_title
           , _cfn_date
           , _cfn_hyperdata) -> ContextForNgrams { .. }) <$> res
  where
    query :: PGS.Query
    query = [sql| SELECT contexts.id, hash_id, typename, user_id, parent_id, name, date, hyperdata
                  FROM contexts
                  JOIN context_node_ngrams ON contexts.id = context_node_ngrams.context_id
                  JOIN nodes_contexts ON contexts.id = nodes_contexts.context_id
                  WHERE nodes_contexts.node_id = ?
                   AND context_node_ngrams.ngrams_id IN ? |]

data ContextForNgramsTerms =
  ContextForNgramsTerms { _cfnt_nodeId     :: NodeId
                        , _cfnt_hash       :: Maybe Hash
                        , _cfnt_nodeTypeId :: NodeTypeId
                        , _cfnt_userId     :: UserId
                        , _cfnt_parentId   :: Maybe ParentId
                        , _cfnt_c_title    :: ContextTitle
                        , _cfnt_date       :: UTCTime
                        , _cfnt_hyperdata  :: HyperdataDocument
                        , _cfnt_score      :: Maybe Double
                        , _cfnt_category   :: Maybe Int }
getContextsForNgramsTerms :: HasNodeError err
                          => NodeId
                          -> [Text]
                          -> DBCmd err [ContextForNgramsTerms]
getContextsForNgramsTerms cId ngramsTerms = do
  res <- runPGSQuery query (cId, PGS.In ngramsTerms)
  pure $ (\( _cfnt_nodeId
           , _cfnt_hash
           , _cfnt_nodeTypeId
           , _cfnt_userId
           , _cfnt_parentId
           , _cfnt_c_title
           , _cfnt_date
           , _cfnt_hyperdata
           , _cfnt_score
           , _cfnt_category) -> ContextForNgramsTerms { .. }) <$> res
  where
    query :: PGS.Query
    query = [sql| SELECT t.id, t.hash_id, t.typename, t.user_id, t.parent_id, t.name, t.date, t.hyperdata, t.score, t.category
                FROM (
                  SELECT DISTINCT ON (contexts.id)
                       contexts.id AS id,
                       hash_id,
                       typename,
                       user_id,
                       parent_id,
                       name,
                       date,
                       hyperdata,
                       nodes_contexts.score AS score,
                       nodes_contexts.category AS category --,
                       -- context_node_ngrams.doc_count AS doc_count
                    FROM contexts
                    JOIN context_node_ngrams ON contexts.id = context_node_ngrams.context_id
                    JOIN nodes_contexts ON contexts.id = nodes_contexts.context_id
                    JOIN ngrams ON context_node_ngrams.ngrams_id = ngrams.id
                    WHERE nodes_contexts.node_id = ?
                     AND ngrams.terms IN ?) t
                   -- ORDER BY t.doc_count DESC
                   ORDER BY t.score DESC
                   |]



-- | Query the `context_node_ngrams` table and return ngrams for given
-- `context_id` and `list_id`.
-- WARNING: `context_node_ngrams` can be outdated. This is because it
-- is expensive to keep all ngrams matching a given context and if
-- someone adds an ngram, we need to recompute its m2m relation to all
-- existing documents.
getContextNgrams :: HasNodeError err
                 => NodeId
                 -> NodeId
                 -> DBCmd err [Text]
getContextNgrams contextId listId = do
  res <- runPGSQuery query (contextId, listId)
  pure $ (\(PGS.Only term) -> term) <$> res

  where
    query :: PGS.Query
    query = [sql| SELECT ngrams.terms
                FROM context_node_ngrams
                JOIN ngrams ON ngrams.id = ngrams_id
                WHERE context_id = ?
                AND node_id = ? |]


-- | Query the `contexts` table and return ngrams for given context_id
-- and list_id that match the search tsvector.
-- NOTE This is poor man's tokenization that is used as a hint for the
-- frontend highlighter.
-- NOTE We prefer `plainto_tsquery` over `phraseto_tsquery` as it is
-- more permissive (i.e. ignores word ordering). See
-- https://www.peterullrich.com/complete-guide-to-full-text-search-with-postgres-and-ecto
getContextNgramsMatchingFTS :: HasNodeError err
                            => ContextId
                            -> NodeId
                            -> DBCmd err [Text]
getContextNgramsMatchingFTS contextId listId = do
  res <- runPGSQuery query (listId, contextId)
  pure $ (\(PGS.Only term) -> term) <$> res

  where
    query :: PGS.Query
    query = [sql| WITH constants AS
                (SELECT ? AS list_id, ? AS context_id),
                ngrams_ids AS
                (SELECT ngrams_id
                 FROM node_stories
                 CROSS JOIN constants
                 WHERE node_id = constants.list_id
                 UNION SELECT ngrams_id
                 FROM node_ngrams
                 CROSS JOIN constants
                 WHERE node_id = constants.list_id)
                SELECT DISTINCT ngrams.terms
                FROM ngrams
                JOIN ngrams_ids ON ngrams_ids.ngrams_id = ngrams.id
                CROSS JOIN constants
                -- JOIN node_ngrams ON node_ngrams.ngrams_id = ngrams.id
                CROSS JOIN contexts
                WHERE contexts.id = constants.context_id
                -- AND node_ngrams.node_id = ?
                AND (contexts.search @@ plainto_tsquery(ngrams.terms)
                  OR contexts.search @@ plainto_tsquery('french', ngrams.terms)) |]
------------------------------------------------------------------------
insertNodeContext :: [NodeContext] -> DBCmd err Int
insertNodeContext ns = mkCmd $ \conn -> fromIntegral <$> (runInsert_ conn
                          $ Insert nodeContextTable ns' rCount (Just DoNothing))
  where
    ns' :: [NodeContextWrite]
    ns' = map (\(NodeContext i n c x y)
                -> NodeContext (sqlInt4 <$> i)
                               (pgNodeId n)
                               (pgNodeId c)
                               (sqlDouble <$> x)
                               (sqlInt4  <$> y)
              ) ns


------------------------------------------------------------------------
type Node_Id    = NodeId
type Context_Id = NodeId

deleteNodeContext :: Node_Id -> Context_Id -> DBCmd err Int
deleteNodeContext n c = mkCmd $ \conn ->
  fromIntegral <$> runDelete_ conn
                              (Delete nodeContextTable
                                      (\(NodeContext _ n_id c_id _ _) -> n_id .== pgNodeId n
                                                                   .&& c_id .== pgNodeId c
                                      )
                                      rCount
                              )

------------------------------------------------------------------------
-- | Favorite management
nodeContextsCategory :: [(CorpusId, DocId, Int)] -> DBCmd err [Int]
nodeContextsCategory inputData = map (\(PGS.Only a) -> a)
                            <$> runPGSQuery catSelect (PGS.Only $ Values fields inputData)
  where
    fields = map (QualifiedIdentifier Nothing) ["int4","int4","int4"]
    catSelect :: PGS.Query
    catSelect = [sql| UPDATE nodes_contexts as nn0
                      SET category = nn1.category
                       FROM (?) as nn1(node_id,context_id,category)
                       WHERE nn0.node_id    = nn1.node_id
                       AND   nn0.context_id = nn1.context_id
                       RETURNING nn1.node_id
                  |]

------------------------------------------------------------------------
-- | Score management
nodeContextsScore :: [(CorpusId, DocId, Int)] -> DBCmd err [Int]
nodeContextsScore inputData = map (\(PGS.Only a) -> a)
                            <$> runPGSQuery catScore (PGS.Only $ Values fields inputData)
  where
    fields = map (QualifiedIdentifier Nothing) ["int4","int4","int4"]
    catScore :: PGS.Query
    catScore = [sql| UPDATE nodes_contexts as nn0
                      SET score = nn1.score
                       FROM (?) as nn1(node_id, context_id, score)
                       WHERE nn0.node_id    = nn1.node_id
                       AND   nn0.context_id = nn1.context_id
                       RETURNING nn1.context_id
                  |]


------------------------------------------------------------------------
selectCountDocs :: HasDBid NodeType => CorpusId -> DBCmd err Int
selectCountDocs cId = runCountOpaQuery (queryCountDocs cId)
  where
    queryCountDocs cId' = proc () -> do
      (c, nc) <- joinInCorpus -< ()
      restrict -< restrictMaybe nc $ \nc' -> (nc' ^. nc_node_id)  .== pgNodeId cId' .&&
                                             (nc' ^. nc_category) .>= sqlInt4 1
      restrict -< (c ^. context_typename) .== sqlInt4 (toDBid NodeDocument)
      returnA  -< c


-- | TODO use UTCTime fast
selectDocsDates :: HasDBid NodeType => CorpusId -> DBCmd err [Text]
selectDocsDates cId =  map (head' "selectDocsDates" . splitOn "-")
                   <$> catMaybes
                   <$> map (view hd_publication_date)
                   <$> selectDocs cId

selectDocs :: HasDBid NodeType => CorpusId -> DBCmd err [HyperdataDocument]
selectDocs cId = runOpaQuery (queryDocs cId)

queryDocs :: HasDBid NodeType => CorpusId -> O.Select (Field SqlJsonb)
queryDocs cId = proc () -> do
  (c, nn)  <- joinInCorpus -< ()
  restrict -< restrictMaybe nn $ \nn' -> (nn' ^. nc_node_id)  .== pgNodeId cId .&&
                                         (nn' ^. nc_category) .>= sqlInt4 1
  restrict -< (c ^. context_typename) .== sqlInt4 (toDBid NodeDocument)
  returnA  -< view (context_hyperdata) c

selectDocNodes :: HasDBid NodeType => CorpusId -> DBCmd err [Context HyperdataDocument]
selectDocNodes cId = runOpaQuery (queryDocNodes cId)

queryDocNodes :: HasDBid NodeType => CorpusId -> O.Select ContextRead
queryDocNodes cId = proc () -> do
  (c, nc) <- joinInCorpus -< ()
  -- restrict -< restrictMaybe nc $ \nc' -> (nc' ^. nc_node_id)  .== pgNodeId cId .&&
  --                                        (nc' ^. nc_category) .>= sqlInt4 1
  restrict -< matchMaybe nc $ \case
    Nothing  -> toFields True
    Just nc' -> (nc' ^. nc_node_id)  .== pgNodeId cId .&&
                (nc' ^. nc_category) .>= sqlInt4 1
  restrict -< (c ^. context_typename) .== sqlInt4 (toDBid NodeDocument)
  returnA -<  c

joinInCorpus :: O.Select (ContextRead, MaybeFields NodeContextRead)
joinInCorpus = proc () -> do
  c <- queryContextTable -< ()
  nc <- optionalRestrict queryNodeContextTable -<
    (\nc' -> (c ^. context_id) .== (nc' ^. nc_context_id))
  returnA -< (c, nc)


joinOn1 :: O.Select (NodeRead, MaybeFields NodeContextRead)
joinOn1 = proc () -> do
  n <- queryNodeTable -< ()
  nc <- optionalRestrict queryNodeContextTable -<
    (\nc' -> (nc' ^. nc_node_id) .== (n ^. node_id))
  returnA -< (n, nc)


------------------------------------------------------------------------
selectPublicContexts :: HasDBid NodeType => (Hyperdata a, DefaultFromField SqlJsonb a)
                  => DBCmd err [(Node a, Maybe Int)]
selectPublicContexts = runOpaQuery (queryWithType NodeFolderPublic)

queryWithType :: HasDBid NodeType => NodeType -> O.Select (NodeRead, MaybeFields (Field SqlInt4))
queryWithType nt = proc () -> do
  (n, nc) <- joinOn1 -< ()
  restrict -< (n ^. node_typename) .== sqlInt4 (toDBid nt)
  returnA  -<  (n, view nc_context_id <$> nc)
