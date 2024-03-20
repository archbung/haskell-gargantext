{-|
Module      : Gargantext.Database.TextSearch
Description : Postgres text search experimentation
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE Arrows            #-}
{-# LANGUAGE LambdaCase         #-}

module Gargantext.Database.Action.Search (
    searchInCorpus
  , searchInCorpusWithContacts
  , searchCountInCorpus
  , searchInCorpusWithNgrams
  , searchDocInDatabase
  ) where

import Control.Arrow (returnA)
import Control.Lens ((^.), view)
import Data.BoolExpr ( BoolExpr(..), Signed(Negative, Positive) )
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Profunctor.Product (p4)
import Data.Set qualified as Set
import Data.Text (unpack)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Gargantext.Core ( Lang(EN), HasDBid(toDBid) )
import Gargantext.Core.Text.Corpus.Query qualified as API
import Gargantext.Core.Text.Terms.Mono.Stem (stem, StemmingAlgorithm(..))
import Gargantext.Core.Types
import Gargantext.Core.Types.Query (IsTrash, Limit, Offset)
import Gargantext.Database.Admin.Types.Hyperdata.Contact ( HyperdataContact(..) )
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument(..) )
import Gargantext.Database.Prelude (runOpaQuery, runCountOpaQuery, DBCmd)
import Gargantext.Database.Query.Facet
import Gargantext.Database.Query.Filter ( limit', offset' )
import Gargantext.Database.Query.Table.Context ( queryContextSearchTable )
import Gargantext.Database.Query.Table.ContextNodeNgrams (queryContextNodeNgramsTable)
import Gargantext.Database.Query.Table.Node ( queryNodeSearchTable, defaultList )
import Gargantext.Database.Query.Table.Node.Error (HasNodeError())
import Gargantext.Database.Query.Table.NodeContext
import Gargantext.Database.Schema.NodeContext_NodeContext ( NodeContext_NodeContextRead, queryNodeContext_NodeContextTable, ncnc_nodecontext2, ncnc_nodecontext1 )
import Gargantext.Database.Schema.Context
import Gargantext.Database.Schema.ContextNodeNgrams (ContextNodeNgramsPoly(..))
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Database.Schema.Node ( NodePolySearch(_ns_hyperdata, _ns_search, _ns_typename, _ns_id) )
import Gargantext.Prelude hiding (groupBy)
import Opaleye hiding (Order)
import Opaleye qualified as O hiding (Order)

--
-- Interpreting a query into a Postgres' TSQuery
--

queryToTsSearch :: API.Query -> Field SqlTSQuery
queryToTsSearch q = sqlToTSQuery $ T.unpack $ API.interpretQuery q transformAST
  where

    -- It's important to understand how things work under the hood: When we perform
    -- a search, we do it on a /ts vector/ in Postgres, which is already stemmed in
    -- lexemes. For example, this:
    --
    -- SELECT to_tsvector('Effects on postpartum on vitamins and minerals in women');
    --
    -- yields:
    --
    -- 'effect':1 'miner':7 'postpartum':3 'vitamin':5 'women':9
    --
    -- As you can see, minimum processing has happened: plurals have been stripped and
    -- what it looks like the Porter stemming has been applied (we get 'miner' instead
    -- of the original /mineral/, for example.
    --
    -- Therefore, in case of exact match searches, we need to perform stemming /regardless/,
    -- and this stemming should ideally match the one performed by Postgres.
    --
    -- Now, if the user is doing a partial match search (like \"~postpartum\" for example)
    -- then we need to stem /AND/ use the \":*\" operator to perform a
    -- sort of fuzzy search. Compare the followings:
    --
    -- SELECT to_tsvector('Effects on postpartum on vitamins and minerals in women') @@ to_tsquery('postpartum');
    -- SELECT to_tsvector('Effects on postpartum on vitamins and minerals in women') @@ to_tsquery('postpart');
    -- SELECT to_tsvector('Effects on postpartum on vitamins and minerals in women') @@ to_tsquery('postpart:*');
    --
    -- The first will match, the second won't, the third will.
    renderQueryTerms :: [API.QueryTerm] -> T.Text
    renderQueryTerms trms = T.intercalate " & " $ trms <&> \case
      API.QT_exact_match (Term term)
        -> stem EN GargPorterAlgorithm term
      API.QT_partial_match (Term term)
        -> stem EN GargPorterAlgorithm term <> ":*"

    transformAST :: BoolExpr [API.QueryTerm] -> T.Text
    transformAST ast = case ast of
      BAnd sub1 sub2
        -> " (" <> transformAST sub1 <> " & " <> transformAST sub2 <> ") "
      BOr sub1 sub2
        -> " (" <> transformAST sub1 <> " | " <> transformAST sub2 <> ") "
      BNot (BConst (Negative term))
        -> transformAST (BConst (Positive term)) -- double negation
      BNot sub
        -> "!" <> transformAST sub
      -- BTrue cannot happen is the query parser doesn't support parsing 'TRUE' alone.
      BTrue
        -> T.empty
      -- BTrue cannot happen is the query parser doesn't support parsing 'FALSE' alone.
      BFalse
        -> T.empty
      BConst (Positive queryTerms)
        -> renderQueryTerms queryTerms
      -- We can handle negatives via `ANDNOT` with itself.
      BConst (Negative queryTerms)
        -> "!" <> renderQueryTerms queryTerms


------------------------------------------------------------------------
searchDocInDatabase :: HasDBid NodeType
                    => ParentId
                    -> Text
                    -> DBCmd err [(NodeId, HyperdataDocument)]
searchDocInDatabase p t = runOpaQuery (queryDocInDatabase p t)
  where
    -- | Global search query where ParentId is Master Node Corpus Id
    queryDocInDatabase :: ParentId -> Text -> O.Select (Column SqlInt4, Column SqlJsonb)
    queryDocInDatabase _p q = proc () -> do
        row <- queryNodeSearchTable -< ()
        restrict -< (_ns_search row)    @@ (sqlPlainToTSQuery (unpack q))
        restrict -< (_ns_typename row) .== (sqlInt4 $ toDBid NodeDocument)
        returnA  -< (_ns_id row, _ns_hyperdata row)

------------------------------------------------------------------------
-- | Search ngrams in documents, ranking them by TF-IDF. We narrow our
-- search only to map/candidate terms.
searchInCorpusWithNgrams :: HasDBid NodeType
               => CorpusId
               -> ListId
               -> IsTrash
               -> NgramsType
               -> [[Text]]
               -> Maybe Offset
               -> Maybe Limit
               -> Maybe OrderBy
               -> DBCmd err [FacetDoc]
searchInCorpusWithNgrams _cId _lId _t _ngt _q _o _l _order = undefined

-- | Compute TF-IDF for all 'ngramIds' in given 'CorpusId'. In this
-- case only the "TF" part makes sense and so we only compute the
-- ratio of "number of times our terms appear in given document" and
-- "number of all terms in document" and return a sorted list of
-- document ids
_tfidfAll :: (HasDBid NodeType, HasNodeError err) => CorpusId -> [Int] -> DBCmd err [Int]
_tfidfAll cId ngramIds = do
  let ngramIdsSet = Set.fromList ngramIds
  lId <- defaultList cId
  docsWithNgrams <- runOpaQuery (_queryListWithNgrams lId ngramIds) :: DBCmd err [(Int, Int, Int)]
  -- NOTE The query returned docs with ANY ngramIds. We need to further
  -- restrict to ALL ngramIds.
  let docsNgramsM =
        Map.fromListWith (Set.union)
            [ (ctxId, Set.singleton ngrams_id)
            | (ctxId, ngrams_id, _) <- docsWithNgrams]
  let docsWithAllNgramsS = Set.fromList $ List.map fst $
        List.filter (\(_, docNgrams) ->
                        ngramIdsSet == Set.intersection ngramIdsSet docNgrams) $ Map.toList docsNgramsM
  let docsWithAllNgrams =
        List.filter (\(ctxId, _, _) ->
                       Set.member ctxId docsWithAllNgramsS) docsWithNgrams
  -- printDebug "[tfidfAll] docsWithAllNgrams" docsWithAllNgrams
  let docsWithCounts = Map.fromListWith (+) [ (ctxId, doc_count)
                                            | (ctxId, _, doc_count) <- docsWithAllNgrams]
  -- printDebug "[tfidfAll] docsWithCounts" docsWithCounts
  let totals = [ ( ctxId
                 , ngrams_id
                 , fromIntegral doc_count :: Double
                 , fromIntegral (fromMaybe 0 $ Map.lookup ctxId docsWithCounts) :: Double)
               | (ctxId, ngrams_id, doc_count) <- docsWithAllNgrams]
  let tfidf_sorted = List.sortOn snd [(ctxId, doc_count/s)
                                     | (ctxId, _, doc_count, s) <- totals]
  pure $ List.map fst $ List.reverse tfidf_sorted

-- | Query for searching the 'context_node_ngrams' table so that we
-- find docs with ANY given 'ngramIds'.
_queryListWithNgrams :: ListId -> [Int] -> Select (Column SqlInt4, Column SqlInt4, Column SqlInt4)
_queryListWithNgrams lId ngramIds = proc () -> do
  row <- queryContextNodeNgramsTable -< ()
  restrict -< (_cnng_node_id row) .== (pgNodeId lId)
  restrict -< in_ (sqlInt4 <$> ngramIds) (_cnng_ngrams_id row)
  returnA -< ( _cnng_context_id row
             , _cnng_ngrams_id row
             , _cnng_doc_count row )
  --returnA -< row
  -- returnA -< ( _cnng_context_id row
  --            , _cnng_node_id row
  --            , _cnng_ngrams_id row
  --            , _cnng_ngramsType row
  --            , _cnng_weight row
  --            , _cnng_doc_count row)


------------------------------------------------------------------------
-- | todo add limit and offset and order
searchInCorpus :: HasDBid NodeType
               => CorpusId
               -> IsTrash
               -> API.Query
               -> Maybe Offset
               -> Maybe Limit
               -> Maybe OrderBy
               -> DBCmd err [FacetDoc]
searchInCorpus cId t q o l order = runOpaQuery
                                 $ filterWith o l order
                                 $ queryInCorpus cId t
                                 $ q

searchCountInCorpus :: HasDBid NodeType
                    => CorpusId
                    -> IsTrash
                    -> API.Query
                    -> DBCmd err Int
searchCountInCorpus cId t q = runCountOpaQuery
                            $ queryInCorpus cId t
                            $ q

queryInCorpus :: HasDBid NodeType
              => CorpusId
              -> IsTrash
              -> API.Query
              -> O.Select FacetDocRead
queryInCorpus cId t q = proc () -> do
  c <- queryContextSearchTable -< ()
  nc <- optionalRestrict queryNodeContextTable -<
    \nc' -> (nc' ^. nc_context_id) .== _cs_id c
  restrict -< (view nc_node_id <$> nc) .=== justFields (pgNodeId cId)
  restrict -< if t
                 then (view nc_category <$> nc) .=== justFields (sqlInt4 0)
                 else matchMaybe (view nc_category <$> nc) $ \case
                        Nothing -> toFields False
                        Just c' -> c' .>= sqlInt4 1
  restrict -< (c ^. cs_search)           @@ queryToTsSearch q
  restrict -< (c ^. cs_typename )       .== sqlInt4 (toDBid NodeDocument)
  returnA  -< FacetDoc { facetDoc_id         = c^.cs_id
                       , facetDoc_created    = c^.cs_date
                       , facetDoc_title      = c^.cs_name
                       , facetDoc_hyperdata  = c^.cs_hyperdata
                       , facetDoc_category   = maybeFieldsToNullable (view nc_category <$> nc)
                       , facetDoc_ngramCount = maybeFieldsToNullable (view nc_score <$> nc)
                       , facetDoc_score      = maybeFieldsToNullable (view nc_score <$> nc)
                       }

------------------------------------------------------------------------
searchInCorpusWithContacts
  :: HasDBid NodeType
  => CorpusId
  -> AnnuaireId
  -> API.Query
  -> Maybe Offset
  -> Maybe Limit
  -> Maybe OrderBy
  -> DBCmd err [FacetPaired Int UTCTime HyperdataContact Int]
searchInCorpusWithContacts cId aId q o l _order =
  runOpaQuery $ limit'   l
              $ offset'  o
              $ orderBy (desc _fp_score)
              $ selectGroup cId aId
              $ q

selectGroup :: HasDBid NodeType
            => CorpusId
            -> AnnuaireId
            -> API.Query
            -> Select FacetPairedRead
selectGroup cId aId q = proc () -> do
  (a, b, c, d) <- aggregate (p4 (groupBy, groupBy, groupBy, O.sum))
                            (selectContactViaDoc cId aId q) -< ()
  returnA -< FacetPaired a b c d


selectContactViaDoc
  :: HasDBid NodeType
  => CorpusId
  -> AnnuaireId
  -> API.Query
  -> SelectArr ()
               ( Field SqlInt4
               , Field SqlTimestamptz
               , Field SqlJsonb
               , Field SqlInt4
               )
selectContactViaDoc cId aId query = proc () -> do
  --(doc, (corpus, (_nodeContext_nodeContext, (annuaire, contact)))) <- queryContactViaDoc -< ()
  (contact, annuaire, _, corpus, doc) <- queryContactViaDoc -< ()
  restrict -< matchMaybe (view cs_search <$> doc) $ \case
    Nothing -> toFields False
    Just s  -> s @@ queryToTsSearch query
  restrict -< (view cs_typename <$> doc)          .=== justFields (sqlInt4 (toDBid NodeDocument))
  restrict -< (view nc_node_id <$> corpus)        .=== justFields (pgNodeId cId)
  restrict -< (view nc_node_id <$> annuaire)      .=== justFields (pgNodeId aId)
  restrict -< (contact ^. context_typename) .== sqlInt4 (toDBid NodeContact)
  returnA  -< ( contact ^. context_id
              , contact ^. context_date
              , contact ^. context_hyperdata
              , sqlInt4 1
              )

queryContactViaDoc :: O.Select ( ContextRead
                               , MaybeFields NodeContextRead
                               , MaybeFields NodeContext_NodeContextRead
                               , MaybeFields NodeContextRead
                               , MaybeFields ContextSearchRead )
queryContactViaDoc = proc () -> do
  contact <- queryContextTable -< ()
  annuaire <- optionalRestrict queryNodeContextTable -<
    \annuaire' -> (annuaire' ^. nc_context_id) .== (contact ^. context_id)
  nodeContext_nodeContext <- optionalRestrict queryNodeContext_NodeContextTable -<
    \ncnc' -> justFields (ncnc' ^. ncnc_nodecontext2) .=== (view nc_id <$> annuaire)
  corpus <- optionalRestrict queryNodeContextTable -<
    \corpus' -> justFields (corpus' ^. nc_id) .=== (view ncnc_nodecontext1 <$> nodeContext_nodeContext)
  doc <- optionalRestrict queryContextSearchTable -<
    \doc' -> justFields (doc' ^. cs_id) .=== (view nc_context_id <$> corpus)

  returnA -< (contact, annuaire, nodeContext_nodeContext, corpus, doc)
