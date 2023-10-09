{-|
Module      : Gargantext.Database.Metrics
Description : Get Metrics from Storage (Database like)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Node API
-}

{-# LANGUAGE QuasiQuotes          #-}

module Gargantext.Database.Action.Metrics
  where

-- import Gargantext.Database.Action.Metrics.NgramsByContext (refreshNgramsMaterialized)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector (Vector)
import Database.PostgreSQL.Simple (Query, Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (toField, Action{-, ToField-})
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Gargantext.API.Ngrams.Tools (filterListWithRoot, groupNodesByNgrams, Diagonal(..), getCoocByNgrams, mapTermListRoot, RootTerm, getRepo)
import Gargantext.API.Ngrams.Types (TabType(..), ngramsTypeFromTabType, NgramsTerm(..))
import Gargantext.Core (HasDBid(toDBid))
import Gargantext.Core.NodeStory hiding (runPGSQuery)
import Gargantext.Core.Text.Metrics (scored, Scored(..), {-localMetrics, toScored-})
import Gargantext.Core.Types (ListType(..), NodeType(..), ContextId, contextId2NodeId)
import Gargantext.Core.Types.Query (Limit(..))
import Gargantext.Database.Action.Metrics.NgramsByContext (getContextsByNgramsOnlyUser{-, getTficfWith-})
import Gargantext.Database.Admin.Config (userMaster)
import Gargantext.Database.Admin.Types.Node (ListId, CorpusId)
import Gargantext.Database.Prelude (runPGSQuery{-, formatPGSQuery-})
import Gargantext.Database.Query.Table.Node.Select
import Gargantext.Prelude

getMetrics :: (HasNodeStory env err m)
           => CorpusId -> ListId -> TabType -> Maybe Limit
           -> m (HashMap NgramsTerm (ListType, Maybe NgramsTerm), Vector (Scored NgramsTerm))
getMetrics cId listId tabType maybeLimit = do
  (ngs, _, myCooc) <- getNgramsCooc cId listId tabType maybeLimit
  -- TODO HashMap
  pure (ngs, scored myCooc)


getNgramsCooc :: (HasNodeStory env err m)
              => CorpusId -> ListId -> TabType -> Maybe Limit
              -> m ( HashMap NgramsTerm (ListType, Maybe NgramsTerm)
                   , HashMap NgramsTerm (Maybe RootTerm)
                   , HashMap (NgramsTerm, NgramsTerm) Int
                   )
getNgramsCooc cId lId tabType maybeLimit = do
  (ngs', ngs) <- getNgrams lId tabType

  lIds <- selectNodesWithUsername NodeList userMaster

  -- FIXME(adn) Audit this, we are converting from a ContextId to a NodeId
  myCooc <- HM.filter (>1) <$> getCoocByNgrams (Diagonal True)
                           <$> HM.map (Set.map contextId2NodeId)
                           <$> groupNodesByNgrams ngs
                           <$> getContextsByNgramsOnlyUser cId
                                                           (lIds <> [lId])
                                                           (ngramsTypeFromTabType tabType)
                                                           (take' maybeLimit $ HM.keys ngs)
  pure $ (ngs', ngs, myCooc)

------------------------------------------------------------------------
------------------------------------------------------------------------
updateNgramsOccurrences :: (HasNodeStory env err m)
                        => CorpusId -> ListId
                        -> m ()
updateNgramsOccurrences cId lId = do
  _ <- mapM (updateNgramsOccurrences' cId lId Nothing) [Terms, Sources, Authors, Institutes]
  pure ()


updateNgramsOccurrences' :: (HasNodeStory env err m)
                         => CorpusId -> ListId -> Maybe Limit -> TabType
                         -> m [Int]
updateNgramsOccurrences' cId lId maybeLimit tabType = do

  result <- getNgramsOccurrences cId lId tabType maybeLimit

  let
    toInsert :: [[Action]]
    toInsert =  map (\(ngramsTerm, score)
                        -> [ toField cId
                           , toField lId
                           , toField $ unNgramsTerm ngramsTerm
                           , toField $ toDBid $ ngramsTypeFromTabType tabType
                           , toField score
                           ]
                      )
       $ HM.toList result

    queryInsert :: Query
    queryInsert = [sql|
                  WITH input(corpus_id, list_id, terms, type_id, weight) AS (?)
                  INSERT into node_node_ngrams (node1_id, node2_id, ngrams_id, ngrams_type, weight)
                  SELECT input.corpus_id,input.list_id,ngrams.id,input.type_id,input.weight FROM input
                  JOIN ngrams on ngrams.terms = input.terms
                  ON CONFLICT (node1_id, node2_id, ngrams_id, ngrams_type)
                  DO UPDATE SET weight = excluded.weight
                  RETURNING 1
                  |]

  let fields = map (\t-> QualifiedIdentifier Nothing t)
             $ map Text.pack ["int4", "int4","text","int4","int4"]

  res <- map (\(Only a) -> a) <$> runPGSQuery queryInsert (Only $ Values fields toInsert)

  -- _ <- map (\(Only a) -> a) <$> runPGSQuery [sql|refresh materialized view context_node_ngrams_view;|] ()
  -- _ <- refreshNgramsMaterialized
  pure res



------------------------------------------------------------------------
-- Used for scores in Ngrams Table
getNgramsOccurrences :: (HasNodeStory env err m)
                     => CorpusId -> ListId -> TabType -> Maybe Limit
                     -> m (HashMap NgramsTerm Int)
getNgramsOccurrences c l t ml = HM.map Set.size <$> getNgramsContexts c l t ml



getNgramsContexts :: (HasNodeStory env err m)
                  => CorpusId -> ListId -> TabType -> Maybe Limit
                  -> m (HashMap NgramsTerm (Set ContextId))
getNgramsContexts cId lId tabType maybeLimit = do
  (_ngs', ngs) <- getNgrams lId tabType
  lIds <- selectNodesWithUsername NodeList userMaster

  -- TODO maybe add an option to group here
  getContextsByNgramsOnlyUser  cId
                               (lIds <> [lId])
                               (ngramsTypeFromTabType tabType)
                               (take' maybeLimit $ HM.keys ngs)



------------------------------------------------------------------------
updateContextScore :: (HasNodeStory env err m)
                   => CorpusId -> ListId
                   -> m [Int]
updateContextScore cId lId = do

  result <- getContextsNgramsScore cId lId Terms MapTerm Nothing

  let
    toInsert :: [[Action]]
    toInsert =  map (\(contextId, score)
                        -> [ toField cId
                           , toField contextId
                           , toField score
                           ]
                      )
       $ Map.toList result

    queryInsert :: Query
    queryInsert = [sql|
                  WITH input(node_id, context_id, score) AS (?)
                    UPDATE nodes_contexts nc
                    SET score = input.score
                    FROM input
                    WHERE nc.node_id = input.node_id
                    AND nc.context_id = input.context_id
                    RETURNING 1
                  |]

  let fields = map (\t-> QualifiedIdentifier Nothing t)
             $ map Text.pack ["int4", "int4","int4"]

  map (\(Only a) -> a) <$> runPGSQuery queryInsert (Only $ Values fields toInsert)




-- Used for scores in Doc Table
getContextsNgramsScore :: (HasNodeStory env err m)
                       => CorpusId -> ListId -> TabType -> ListType -> Maybe Limit
                       -> m (Map ContextId Int)
getContextsNgramsScore cId lId tabType listType maybeLimit
 = Map.map Set.size <$> getContextsNgrams cId lId tabType listType maybeLimit

getContextsNgrams :: (HasNodeStory env err m)
                  => CorpusId -> ListId -> TabType -> ListType -> Maybe Limit
                  -> m (Map ContextId (Set NgramsTerm))
getContextsNgrams cId lId tabType listType maybeLimit = do
  (ngs', ngs) <- getNgrams lId tabType
  lIds <- selectNodesWithUsername NodeList userMaster

  result <- groupNodesByNgrams ngs <$> getContextsByNgramsOnlyUser
                               cId
                               (lIds <> [lId])
                               (ngramsTypeFromTabType tabType)
                               ( take' maybeLimit
                               $ HM.keys
                               $ HM.filter (\v -> fst v == listType) ngs'
                               )
  -- printDebug "getCoocByNgrams" result
  pure $ Map.fromListWith (<>)
       $ List.concat
       $ map (\(ng, contexts) -> List.zip (Set.toList contexts) (List.cycle [Set.singleton ng]))
       $ HM.toList result


------------------------------------------------------------------------
------------------------------------------------------------------------


getNgrams :: (HasNodeStory env err m)
            => ListId -> TabType
            -> m ( HashMap NgramsTerm (ListType, Maybe NgramsTerm)
                 , HashMap NgramsTerm (Maybe RootTerm)
                 )
getNgrams lId tabType = do

  lists <- mapTermListRoot [lId] (ngramsTypeFromTabType tabType) <$> getRepo [lId]
  -- TODO filterListWithRoot [MapTerm, StopTerm, CandidateTerm] lists
  let maybeSyn = HM.unions $ map (\t -> filterListWithRoot t lists)
                                 [[MapTerm], [StopTerm], [CandidateTerm]]
  pure (lists, maybeSyn)

-- Some useful Tools
take' :: Maybe Limit -> [a] -> [a]
take' Nothing  xs = xs
take' (Just n) xs = take (getLimit n) xs
