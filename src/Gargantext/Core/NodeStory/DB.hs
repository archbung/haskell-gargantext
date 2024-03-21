{-|
Module      : Gargantext.Core.NodeStory.DB
Description : NodeStory DB functions
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

module Gargantext.Core.NodeStory.DB
  ( nodeExists
  , getNodesIdWithType
  , getNodesArchiveHistory
  , insertNodeArchiveHistory
  , nodeStoriesQuery
  , insertArchiveStateList
  , deleteArchiveStateList
  , updateArchiveStateList
  , updateNodeStoryVersion )
where

import Control.Lens ((^.))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Map.Strict.Patch qualified as PM
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Gargantext.API.Ngrams.Types
import Gargantext.Core (toDBid)
import Gargantext.Core.NodeStory.Types ( a_state, a_version, ArchiveList, ArchiveStateList, NgramsStatePatch' )
import Gargantext.Core.Text.Ngrams (NgramsType)
import Gargantext.Database.Admin.Types.Node ( NodeId(..), NodeType )
import Gargantext.Database.Admin.Config ()
import Gargantext.Database.Schema.Ngrams ()
import Gargantext.Prelude hiding (to)
import Gargantext.Prelude.Database


nodeExists :: PGS.Connection -> NodeId -> IO Bool
nodeExists c nId = (== [PGS.Only True])
  <$> runPGSQuery c [sql| SELECT true FROM nodes WHERE id = ? LIMIT 1 |]
                    (PGS.Only nId)

getNodesIdWithType :: PGS.Connection -> NodeType -> IO [NodeId]
getNodesIdWithType c nt = do
  ns <- runPGSQuery c query (PGS.Only $ toDBid nt)
  pure $ map (\(PGS.Only nId) -> UnsafeMkNodeId nId) ns
  where
    query :: PGS.Query
    query = [sql| SELECT id FROM nodes WHERE typename = ? |]


-- /!\ This function is using an hard coded parameter
-- which depends on the Ngrams List Flow
-- Version > 5 is hard coded because by default
-- first version of history of manual change is 6
getNodesArchiveHistory :: PGS.Connection
                       -> [NodeId]
                       -> IO [(NodeId, (Map NgramsType [HashMap NgramsTerm NgramsPatch]))]
getNodesArchiveHistory c nodesId = do
  as <- runPGSQuery c query (PGS.Only $ Values fields nodesId)
                            :: IO [(Int, NgramsType, NgramsTerm, NgramsPatch)]

  pure $ map (\(nId, ngramsType, terms, patch)
               -> ( UnsafeMkNodeId nId
                  , Map.singleton ngramsType [HashMap.singleton terms patch]
                  )
             ) as
  where

    fields = [QualifiedIdentifier Nothing "int4"]
    query :: PGS.Query
    query = [sql| WITH nodes_id(nid) as (?)
                    SELECT node_id, ngrams_type_id, terms, patch
                    FROM node_story_archive_history
                    JOIN ngrams ON ngrams.id = ngrams_id
                    JOIN nodes_id n ON node_id = n.nid
                    WHERE version > 5
                    ORDER BY (version, node_story_archive_history.id) DESC
            |]


insertNodeArchiveHistory :: PGS.Connection -> NodeId -> Version -> [NgramsStatePatch'] -> IO ()
insertNodeArchiveHistory _ _ _ [] = pure ()
insertNodeArchiveHistory c nodeId version (h:hs) = do
  let tuples = mconcat $ (\(nType, NgramsTablePatch patch) ->
                           (\(term, p) ->
                              (nodeId, nType, term, p)) <$> PM.toList patch) <$> PM.toList h :: [(NodeId, NgramsType, NgramsTerm, NgramsPatch)]
  tuplesM <- mapM (\(nId, nType, term, patch) -> do
                      [PGS.Only ngramsId] <- runPGSReturning c qInsert [PGS.Only term] :: IO [PGS.Only Int]
                      pure (nId, nType, ngramsId, term, patch)
                  ) tuples :: IO [(NodeId, NgramsType, Int, NgramsTerm, NgramsPatch)]
  _ <- runPGSExecuteMany c query $ ((\(nId, nType, termId, _term, patch) -> (nId, nType, termId, patch, version)) <$> tuplesM)
  _ <- insertNodeArchiveHistory c nodeId version hs
  pure ()
  where
    qInsert :: PGS.Query
    qInsert = [sql|INSERT INTO ngrams (terms) VALUES (?)
                  ON CONFLICT (terms) DO UPDATE SET terms = excluded.terms
                  RETURNING id|]

    -- https://stackoverflow.com/questions/39224438/postgresql-insert-if-foreign-key-exists
    query :: PGS.Query
    query = [sql| INSERT INTO node_story_archive_history(node_id, ngrams_type_id, ngrams_id, patch, version)
                VALUES (?, ?, ?, ?, ?)
                |]

      
nodeStoriesQuery :: PGS.Query
nodeStoriesQuery = [sql| SELECT version, ngrams_type_id, terms, ngrams_repo_element
                           FROM node_stories
                           JOIN ngrams ON ngrams.id = ngrams_id
                           WHERE node_id = ?
                           |]


-- Archive


insertArchiveStateList :: PGS.Connection -> NodeId -> Version -> ArchiveStateList -> IO ()
insertArchiveStateList c nodeId version as = do
  mapM_ performInsert as
  where
    performInsert (ngramsType, ngrams, ngramsRepoElement) = do
      [PGS.Only ngramsId] <- tryInsertTerms ngrams
      _ <- case ngramsRepoElement ^. nre_root of
        Nothing -> pure []
        Just r -> tryInsertTerms r
      mapM_ tryInsertTerms $ ngramsRepoElement ^. nre_children
      runPGSExecute c query (nodeId, ngramsId, version, ngramsType, ngramsRepoElement)
    
    tryInsertTerms :: NgramsTerm -> IO [PGS.Only Int]
    tryInsertTerms t = runPGSReturning c qInsert [PGS.Only t]
    
    qInsert :: PGS.Query
    qInsert = [sql|INSERT INTO ngrams (terms) VALUES (?)
                  ON CONFLICT (terms) DO UPDATE SET terms = excluded.terms
                  RETURNING id|]
    
    query :: PGS.Query
    query = [sql|INSERT INTO node_stories(node_id, ngrams_id, version, ngrams_type_id, ngrams_repo_element)
                VALUES (?, ?, ?, ?, ? :: jsonb)
                |]

deleteArchiveStateList :: PGS.Connection -> NodeId -> ArchiveStateList -> IO ()
deleteArchiveStateList c nodeId as = do
  mapM_ (\(nt, n, _) -> runPGSExecute c query (nodeId, nt, n)) as
  where
    query :: PGS.Query
    query = [sql| DELETE FROM node_stories
                WHERE node_id = ? AND ngrams_type_id = ?
                  AND ngrams_id IN (SELECT id FROM ngrams WHERE terms = ?)
                  |]

updateArchiveStateList :: PGS.Connection -> NodeId -> Version -> ArchiveStateList -> IO ()
updateArchiveStateList c nodeId version as = do
  let params = (\(nt, n, nre) -> (nre, version, nodeId, nt, n)) <$> as
  mapM_ (runPGSExecute c query) params
  where
    query :: PGS.Query
    query = [sql| UPDATE node_stories
                SET ngrams_repo_element = ?, version = ?
                WHERE node_id = ? AND ngrams_type_id = ?
                  AND ngrams_id IN (SELECT id FROM ngrams WHERE terms = ?)
                  |]


updateNodeStoryVersion :: PGS.Connection -> NodeId -> ArchiveList -> IO ()
updateNodeStoryVersion c nodeId newArchive = do
  let ngramsTypes = Map.keys $ newArchive ^. a_state
  mapM_ (\nt -> runPGSExecute c query (newArchive ^. a_version, nodeId, nt)) ngramsTypes
  where
    query :: PGS.Query
    query = [sql|UPDATE node_stories
                SET version = ?
                WHERE node_id = ?
                AND ngrams_type_id = ?|]
