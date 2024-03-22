{-|
Module      : Gargantext.Core.NodeStory
Description : NodeStory
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

A Node Story is a Map between NodeId and an Archive (with state,
version and history) for that node.

Couple of words on how this is implemented.

First version used files which stored Archive for each NodeId in a
separate .cbor file.

For performance reasons, it is rewritten to use the DB.

The table `node_stories` contains two columns: `node_id` and
`archive`.

Next, it was observed that `a_history` in `Archive` takes much
space. So a new table was created, `node_story_archive_history` with
columns: `node_id`, `ngrams_type_id`, `patch`. This is because each
history item is in fact a map from `NgramsType` to `NgramsTablePatch`
(see the `NgramsStatePatch'` type).

Moreover, since in `G.A.Ngrams.commitStatePatch` we use current state
only, with only recent history items, I concluded that it is not
necessary to load whole history into memory. Instead, it is kept in DB
(history is immutable) and only recent changes are added to
`a_history`. Then that record is cleared whenever `Archive` is saved.

Please note that

TODO:
- remove
- filter
- charger les listes
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

module Gargantext.Core.NodeStory
  ( module Gargantext.Core.NodeStory.Types
  , getNodesArchiveHistory
  , Archive(..)
  , nodeExists
  , getNodesIdWithType
  , fromDBNodeStoryEnv
  , upsertNodeStories
  -- , getNodeStory
  , getNodeStory'
  , nodeStoriesQuery
  , currentVersion
  , archiveStateFromList
  , archiveStateToList
  , fixNodeStoryVersions
  , fixChildrenDuplicatedAsParents
  , getParentsChildren )
where

import Control.Lens ((^.), (.~), (%~), non, _Just, at, over, view)
import Data.Map.Strict qualified as Map
import Data.Pool (Pool, withResource)
import Data.Set qualified as Set
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField qualified as PGS
import Gargantext.API.Ngrams.Types
import Gargantext.Core.NodeStory.DB
import Gargantext.Core.NodeStory.Types
import Gargantext.Core.Text.Ngrams qualified as Ngrams
import Gargantext.Database.Admin.Types.Node ( ListId, NodeId(..) )
import Gargantext.Database.Admin.Config ()
import Gargantext.Database.Prelude (HasConnectionPool(..))
import Gargantext.Prelude hiding (to)
import Gargantext.Prelude.Database ( runPGSAdvisoryXactLock, runPGSExecute, runPGSQuery )


getNodeStory' :: PGS.Connection -> NodeId -> IO ArchiveList
getNodeStory' c nId = do
  --res <- withResource pool $ \c -> runSelect c query :: IO [NodeStoryPoly NodeId Version Int Int NgramsRepoElement]
  res <- runPGSQuery c nodeStoriesQuery (PGS.Only $ PGS.toField nId) :: IO [(Version, Ngrams.NgramsType, NgramsTerm, NgramsRepoElement)]
  -- We have multiple rows with same node_id and different (ngrams_type_id, ngrams_id).
  -- Need to create a map: {<node_id>: {<ngrams_type_id>: {<ngrams_id>: <data>}}}
  let dbData = map (\(version, ngramsType, ngrams, ngrams_repo_element) ->
                      Archive { _a_version = version
                              , _a_history = []
                              , _a_state   = Map.singleton ngramsType $ Map.singleton ngrams ngrams_repo_element }) res
  -- NOTE Sanity check: all versions in the DB should be the same
   -- TODO Maybe redesign the DB so that `node_stories` has only
   -- `node_id`, `version` and there is a M2M table
   -- `node_stories_ngrams` without the `version` colum? Then we would
   -- have `version` in only one place.

{-
  let versionsS = Set.fromList $ map (\a -> a ^. a_version) dbData
  if Set.size versionsS > 1 then
    panic $ Text.pack $ "[getNodeStory] versions for " <> show nodeId <> " differ! " <> show versionsS
  else
    pure ()
-}

  pure $ foldl' combine initArchive dbData
  where
    -- NOTE (<>) for Archive doesn't concatenate states, so we have to use `combine`
    combine a1 a2 = a1 & a_state %~ combineState (a2 ^. a_state)
                       & a_version .~ (a2 ^. a_version)  -- version should be updated from list, not taken from the empty Archive


getNodeStory :: PGS.Connection -> NodeId -> IO NodeListStory
getNodeStory c nId = do
  a <- getNodeStory' c nId
  pure $ NodeStory $ Map.singleton nId a

-- |Functions to convert archive state (which is a `Map NgramsType
--  (Map NgramsTerm NgramsRepoElement`)) to/from a flat list
archiveStateToList :: NgramsState' -> ArchiveStateList
archiveStateToList s = mconcat $ (\(nt, ntm) -> (\(n, nre) -> (nt, n, nre)) <$> Map.toList ntm) <$> Map.toList s

archiveStateFromList :: ArchiveStateList -> NgramsState'
archiveStateFromList l = Map.fromListWith (<>) $ (\(nt, t, nre) -> (nt, Map.singleton t nre)) <$> l

archiveStateSet :: ArchiveStateList -> ArchiveStateSet
archiveStateSet lst = Set.fromList $ (\(nt, term, _) -> (nt, term)) <$> lst

archiveStateListFilterFromSet :: ArchiveStateSet -> ArchiveStateList -> ArchiveStateList
archiveStateListFilterFromSet set =
  filter (\(nt, term, _) -> Set.member (nt, term) set)

-- | This function inserts whole new node story and archive for given node_id.
insertNodeStory :: PGS.Connection -> NodeId -> ArchiveList -> IO ()
insertNodeStory c nId a = do
  insertArchiveStateList c nId (a ^. a_version) (archiveStateToList $ a ^. a_state)

-- | This function updates the node story and archive for given node_id.
updateNodeStory :: PGS.Connection -> NodeId -> ArchiveList -> ArchiveList -> IO ()
updateNodeStory c nodeId currentArchive newArchive = do
  -- STEPS

  -- 0. We assume we're inside an advisory lock

  -- 1. Find differences (inserts/updates/deletes)
  let currentList = archiveStateToList $ currentArchive ^. a_state
  let newList = archiveStateToList $ newArchive ^. a_state
  let currentSet = archiveStateSet currentList
  let newSet = archiveStateSet newList

  -- printDebug "[updateNodeStory] new - current = " $ Set.difference newSet currentSet
  let inserts = archiveStateListFilterFromSet (Set.difference newSet currentSet) newList
  -- printDebug "[updateNodeStory] inserts" inserts

  -- printDebug "[updateNodeStory] current - new" $ Set.difference currentSet newSet
  let deletes = archiveStateListFilterFromSet (Set.difference currentSet newSet) currentList
  -- printDebug "[updateNodeStory] deletes" deletes

  -- updates are the things that are in new but not in current
  let commonSet = Set.intersection currentSet newSet
  let commonNewList = archiveStateListFilterFromSet commonSet newList
  let commonCurrentList = archiveStateListFilterFromSet commonSet currentList
  let updates = Set.toList $ Set.difference (Set.fromList commonNewList) (Set.fromList commonCurrentList)
  -- printDebug "[updateNodeStory] updates" $ Text.unlines $ (Text.pack . show) <$> updates

  -- 2. Perform inserts/deletes/updates
  -- printDebug "[updateNodeStory] applying inserts" inserts
  insertArchiveStateList c nodeId (newArchive ^. a_version) inserts
  --printDebug "[updateNodeStory] insert applied" ()
    --TODO Use currentArchive ^. a_version in delete and report error
  -- if entries with (node_id, ngrams_type_id, ngrams_id) but
  -- different version are found.
  deleteArchiveStateList c nodeId deletes
  --printDebug "[updateNodeStory] delete applied" ()
  updateArchiveStateList c nodeId (newArchive ^. a_version) updates
  --printDebug "[updateNodeStory] update applied" ()

  pure ()
  -- where
  --   update = Update { uTable      = nodeStoryTable
  --                   , uUpdateWith = updateEasy (\(NodeStoryDB { node_id }) ->
  --                                                 NodeStoryDB { archive = sqlValueJSONB $ Archive { _a_history = emptyHistory
  --                                                                                                                               , ..}
  --                                                                                           , .. })
  --                   , uWhere      = (\row -> node_id row .== sqlInt4 nId)
  --                   , uReturning  = rCount }

upsertNodeStories :: PGS.Connection -> NodeId -> ArchiveList -> IO ()
upsertNodeStories c nodeId newArchive = do
  -- printDebug "[upsertNodeStories] START nId" nId
  PGS.withTransaction c $ do
    -- printDebug "[upsertNodeStories] locking nId" nId
    runPGSAdvisoryXactLock c (_NodeId nodeId)

    (NodeStory m) <- getNodeStory c nodeId
    case Map.lookup nodeId m of
      Nothing -> do
        _ <- insertNodeStory c nodeId newArchive
        pure ()
      Just currentArchive  -> do
        _ <- updateNodeStory c nodeId currentArchive newArchive
        pure ()

    -- 3. Now we need to set versions of all node state to be the same
    updateNodeStoryVersion c nodeId newArchive

    -- printDebug "[upsertNodeStories] STOP nId" nId

-- | Returns a `NodeListStory`, updating the given one for given `NodeId`
nodeStoryInc :: PGS.Connection -> NodeListStory -> NodeId -> IO NodeListStory
nodeStoryInc c ns@(NodeStory nls) nId = do
  case Map.lookup nId nls of
    Nothing -> do
      NodeStory nls' <- getNodeStory c nId
      pure $ NodeStory $ Map.unionWith archiveAdvance nls' nls
    Just _ -> pure ns

-- | NgramsRepoElement contains, in particular, `nre_list`,
-- `nre_parent` and `nre_children`. We want to make sure that all
-- children entries (i.e. ones that have `nre_parent`) have the same
-- `list` as their parent entry.
fixChildrenInNgrams :: NgramsState' -> NgramsState'
fixChildrenInNgrams ns = archiveStateFromList $ nsParents <> nsChildrenFixed
  where
    (nsParents, nsChildren) = getParentsChildren ns
    parentNtMap = Map.fromList $ (\(_nt, t, nre) -> (t, nre ^. nre_list)) <$> nsParents

    nsChildrenFixed = (\(nt, t, nre) ->
                         ( nt
                         , t
                         , nre & nre_list %~
                            (\l -> parentNtMap ^. at (nre ^. nre_parent . _Just) . non l)
                         )
                      ) <$> nsChildren

-- | (#281) Sometimes, when we upload a new list, a child can be left
-- without a parent. Find such ngrams and set their 'root' and
-- 'parent' to 'Nothing'.
fixChildrenWithNoParent :: NgramsState' -> NgramsState'
fixChildrenWithNoParent ns = archiveStateFromList $ nsParents <> nsChildrenFixed
  where
    (nsParents, nsChildren) = getParentsChildren ns
    parentNtMap = Map.fromList $ (\(_nt, t, nre) -> (t, nre ^. nre_children & mSetToSet)) <$> nsParents

    nsChildrenFixFunc (nt, t, nre) =
      ( nt
      , t
      , nre { _nre_root = root
            , _nre_parent = parent }
      )
      where
        (root, parent) = case parentNtMap ^. at (nre ^. nre_parent . _Just) . _Just . at t of
          Just _ -> (nre ^. nre_root, nre ^. nre_parent)
          Nothing -> (Nothing, Nothing)

    nsChildrenFixed = nsChildrenFixFunc <$> nsChildren

-- | Sometimes children can also become parents (e.g. #313). Find such
-- | children and remove them from the list.
fixChildrenDuplicatedAsParents :: NgramsState' -> NgramsState'
fixChildrenDuplicatedAsParents ns = archiveStateFromList $ nsChildren <> nsParentsFixed
  where
    (nsParents, nsChildren) = getParentsChildren ns
    parentNtMap = Map.fromList $ (\(_nt, t, nre) -> (t, nre ^. nre_children & mSetToSet)) <$> nsParents
    parentsSet = Set.fromList $ Map.keys parentNtMap

    nsParentsFixed = (\(nt, t, nre) ->
                       ( nt
                       , t
                       , over nre_children
                             (\c -> mSetFromSet $ Set.difference (mSetToSet c) parentsSet)
                             nre ) ) <$> nsParents

getParentsChildren :: NgramsState' -> (ArchiveStateList, ArchiveStateList)
getParentsChildren ns = (nsParents, nsChildren)
  where
    nls = archiveStateToList ns

    nsParents = filter (\(_nt, _t, nre) -> isNothing $ nre ^. nre_parent) nls
    nsChildren = filter (\(_nt, _t, nre) -> isJust $ nre ^. nre_parent) nls

------------------------------------

fromDBNodeStoryEnv :: Pool PGS.Connection -> IO NodeStoryEnv
fromDBNodeStoryEnv pool = do
  -- tvar <- nodeStoryVar pool Nothing []
  let saver_immediate nId a = do
        -- ns <- atomically $
        --       readTVar tvar
        --       -- fix children so their 'list' is the same as their parents'
        --       >>= pure . fixChildrenTermTypes
        --       -- fix children that don't have a parent anymore
        --       >>= pure . fixChildrenWithNoParent
        --       >>= writeTVar tvar
        --       >> readTVar tvar
        withResource pool $ \c -> do
          --printDebug "[mkNodeStorySaver] will call writeNodeStories, ns" ns
          -- writeNodeStories c $ fixChildrenWithNoParent $ fixChildrenTermTypes ns

          -- |NOTE Fixing a_state is kinda a hack. We shouldn't land
          -- |with bad state in the first place.
          upsertNodeStories c nId $
            a & a_state %~ (
                       fixChildrenDuplicatedAsParents
                    .  fixChildrenInNgrams
                    . fixChildrenWithNoParent
                    )
  let archive_saver_immediate nId a = withResource pool $ \c -> do
        insertNodeArchiveHistory c nId (a ^. a_version) $ reverse $ a ^. a_history
        pure $ a & a_history .~ []
        -- mapM_ (\(nId, a) -> do
        --           insertNodeArchiveHistory c nId (a ^. a_version) $ reverse $ a ^. a_history
        --       ) $ Map.toList nls
        -- pure $ clearHistory ns

  pure $ NodeStoryEnv { _nse_saver_immediate = saver_immediate
                      , _nse_archive_saver_immediate = archive_saver_immediate
                      , _nse_getter = \nId -> withResource pool $ \c ->
                          getNodeStory' c nId
                      , _nse_getter_multi = \nIds -> withResource pool $ \c ->
                          foldM (nodeStoryInc c) (NodeStory Map.empty) nIds
                      }

currentVersion :: (HasNodeStory env err m) => ListId -> m Version
currentVersion listId = do
  pool <- view connPool
  nls <- liftBase $ withResource pool $ \c -> liftBase $ getNodeStory c listId
  pure $ nls ^. unNodeStory . at listId . _Just . a_version


-----------------------------------------

-- | To be called from the REPL
fixNodeStoryVersions :: (HasNodeStory env err m) => m ()
fixNodeStoryVersions = do
  pool <- view connPool
  _ <- liftBase $ withResource pool $ \c -> liftBase $ PGS.withTransaction c $ do
    nIds <- runPGSQuery c [sql| SELECT id FROM nodes WHERE ? |] (PGS.Only True) :: IO [PGS.Only Int64]
    -- printDebug "[fixNodeStoryVersions] nIds" nIds
    mapM_ (\(PGS.Only nId) -> do
        -- printDebug "[fixNodeStoryVersions] nId" nId
        updateVer c Ngrams.Authors nId

        updateVer c Ngrams.Institutes nId

        updateVer c Ngrams.Sources nId

        updateVer c Ngrams.NgramsTerms nId

        pure ()
      ) nIds
  pure ()
  where
    maxVerQuery :: PGS.Query
    maxVerQuery = [sql| SELECT max(version)
                      FROM node_stories
                      WHERE node_id = ?
                        AND ngrams_type_id = ? |]
    updateVerQuery :: PGS.Query
    updateVerQuery = [sql| UPDATE node_stories
                         SET version = ?
                         WHERE node_id = ?
                           AND ngrams_type_id = ? |]
    updateVer :: PGS.Connection -> Ngrams.NgramsType -> Int64 -> IO ()
    updateVer c ngramsType nId = do
      maxVer <- runPGSQuery c maxVerQuery (nId, ngramsType) :: IO [PGS.Only (Maybe Int64)]
      case maxVer of
        [] -> pure ()
        [PGS.Only Nothing] -> pure ()
        [PGS.Only (Just maxVersion)] -> do
          _ <- runPGSExecute c updateVerQuery (maxVersion, nId, ngramsType)
          pure ()
        _ -> panicTrace "Should get only 1 result!"

-----------------------------------------

-- DEPRECATED


-- nodeStoryVar :: Pool PGS.Connection
--              -> Maybe (TVar NodeListStory)
--              -> [NodeId]
--              -> IO (TVar NodeListStory)
-- nodeStoryVar pool Nothing nIds = do
--   state' <- withResource pool $ \c -> nodeStoryIncrementalRead c Nothing nIds
--   atomically $ newTVar state'
-- nodeStoryVar pool (Just tv) nIds = do
--   nls <- atomically $ readTVar tv
--   nls' <- withResource pool
--       $ \c -> nodeStoryIncrementalRead c (Just nls) nIds
--   _ <- atomically $ writeTVar tv nls'
--   pure tv

-- clearHistory :: NodeListStory -> NodeListStory
-- clearHistory (NodeStory ns) = NodeStory $ ns & (traverse . a_history) .~ emptyHistory
--   where
--     emptyHistory = [] :: [NgramsStatePatch']


-- fixChildrenWithNoParent :: NodeListStory -> NodeListStory
-- fixChildrenWithNoParent (NodeStory nls) =
--   NodeStory $ Map.fromList [ (nId, a & a_state %~ fixChildrenWithNoParentStatePatch)
--                            | (nId, a) <- Map.toList nls ]


-- fixChildrenTermTypes :: NodeListStory -> NodeListStory
-- fixChildrenTermTypes (NodeStory nls) =
--   NodeStory $ Map.fromList [ (nId, a & a_state %~ fixChildrenInNgramsStatePatch)
--                            | (nId, a) <- Map.toList nls ]


-- nodeStoryIncrementalRead :: PGS.Connection -> Maybe NodeListStory -> [NodeId] -> IO NodeListStory
-- nodeStoryIncrementalRead _ Nothing [] = pure $ NodeStory Map.empty
-- nodeStoryIncrementalRead c Nothing (ni:ns) = do
--   m <- getNodeStory c ni
--   nodeStoryIncrementalRead c (Just m) ns
-- nodeStoryIncrementalRead c (Just nls) ns = foldM (\m n -> nodeStoryInc c m n) nls ns

-- nodeStoryDec :: Pool PGS.Connection -> NodeListStory -> NodeId -> IO NodeListStory
-- nodeStoryDec pool ns@(NodeStory nls) ni = do
--   case Map.lookup ni nls of
--     Nothing -> do
--       _ <- nodeStoryRemove pool ni
--       pure ns
--     Just _ -> do
--       let ns' = Map.filterWithKey (\k _v -> k /= ni) nls
--       _ <- nodeStoryRemove pool ni
--       pure $ NodeStory ns'
------------------------------------


-- writeNodeStories :: PGS.Connection -> NodeListStory -> IO ()
-- writeNodeStories c (NodeStory nls) = do
--   mapM_ (\(nId, a) -> upsertNodeStories c nId a) $ Map.toList nls


-- nodeStoryRemove :: Pool PGS.Connection -> NodeId -> IO Int64
-- nodeStoryRemove pool (NodeId nId) = withResource pool $ \c -> runDelete c delete
--   where
--     delete = Delete { dTable     = nodeStoryTable
--                     , dWhere     = (\row -> node_id row .== sqlInt4 nId)
--                     , dReturning = rCount }
