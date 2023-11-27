{-|
Module      : Gargantext.Core.NodeStory
Description : Node API generation
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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Core.NodeStory
  ( HasNodeStory
  , HasNodeStoryEnv
  , hasNodeStory
  , HasNodeStoryVar
  , hasNodeStoryVar
  , HasNodeStoryImmediateSaver
  , hasNodeStoryImmediateSaver
  , HasNodeArchiveStoryImmediateSaver
  , hasNodeArchiveStoryImmediateSaver
  , NodeStory(..)
  , NgramsStatePatch'
  , NodeListStory
  , ArchiveList
  , initNodeListStoryMock
  , NodeStoryEnv(..)
  , initNodeStory
  , nse_getter
  , nse_saver_immediate
  , nse_archive_saver_immediate
  , nse_var
  , unNodeStory
  , getNodesArchiveHistory
  , Archive(..)
  , initArchive
  , archiveAdvance
  , unionArchives
  , a_history
  , a_state
  , a_version
  , nodeExists
  , runPGSQuery
  , runPGSAdvisoryLock
  , runPGSAdvisoryUnlock
  , runPGSAdvisoryXactLock
  , getNodesIdWithType
  , fromDBNodeStoryEnv
  , upsertNodeStories
  , getNodeStory
  , nodeStoriesQuery
  , currentVersion
  , archiveStateFromList
  , archiveStateToList
  , fixNodeStoryVersions )
where

import Codec.Serialise.Class
import Control.Lens (makeLenses, Getter, (^.), (.~), (%~), non, _Just, at, view)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson hiding ((.=), decode)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Map.Strict.Patch qualified as PM
import Data.Monoid
import Data.Pool (Pool, withResource)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Semigroup
import Data.Set qualified as Set
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.FromField (FromField(fromField), fromJSONField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField qualified as PGS
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import GHC.Conc (TVar, newTVar, readTVar, writeTVar)
import Gargantext.API.Ngrams.Types
import Gargantext.Core (toDBid)
import Gargantext.Core.Errors.Types (panicTrace)
import Gargantext.Core.Types (ListId, NodeId(..), NodeType)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Admin.Config ()
import Gargantext.Database.Prelude (DbCmd', HasConnectionPool(..))
import Gargantext.Database.Query.Table.Ngrams qualified as TableNgrams
import Gargantext.Database.Query.Table.Node.Error (HasNodeError())
import Gargantext.Database.Schema.Ngrams (NgramsType)
import Gargantext.Prelude hiding (to)
import Gargantext.Prelude.Database
import Opaleye (DefaultFromField(..), SqlJsonb, fromPGSFromField)

------------------------------------------------------------------------
data NodeStoryEnv = NodeStoryEnv
  { _nse_var    :: !(TVar NodeListStory)
  , _nse_saver_immediate :: !(IO ())
  , _nse_archive_saver_immediate :: !(NodeListStory -> IO NodeListStory)
  , _nse_getter :: !([NodeId] -> IO (TVar NodeListStory))
  --, _nse_cleaner :: !(IO ()) -- every 12 hours: cleans the repos of unused NodeStories
  -- , _nse_lock  :: !FileLock -- TODO (it depends on the option: if with database or file only)
  }
  deriving (Generic)

type HasNodeStory env err m = ( DbCmd' env err m
                              , MonadReader env m
                              , MonadError  err m
                              , HasNodeStoryEnv env
                              , HasNodeError err
                              )

class (HasNodeStoryVar env, HasNodeStoryImmediateSaver env)
  => HasNodeStoryEnv env where
    hasNodeStory :: Getter env NodeStoryEnv

class HasNodeStoryVar env where
  hasNodeStoryVar :: Getter env ([NodeId] -> IO (TVar NodeListStory))

class HasNodeStoryImmediateSaver env where
  hasNodeStoryImmediateSaver :: Getter env (IO ())

class HasNodeArchiveStoryImmediateSaver env where
  hasNodeArchiveStoryImmediateSaver :: Getter env (NodeListStory -> IO NodeListStory)

------------------------------------------------------------------------

{- | Node Story for each NodeType where the Key of the Map is NodeId
  TODO : generalize for any NodeType, let's start with NodeList which
  is implemented already
-}
newtype NodeStory s p = NodeStory { _unNodeStory :: Map NodeId (Archive s p) }
  deriving (Generic, Show, Eq)

instance (FromJSON s, FromJSON p) => FromJSON (NodeStory s p)
instance (ToJSON s, ToJSON p) => ToJSON (NodeStory s p)
instance (Serialise s, Serialise p) => Serialise (NodeStory s p)

data Archive s p = Archive
  { _a_version           :: !Version
  , _a_state             :: !s
  , _a_history           :: ![p]
    -- first patch in the list is the most recent
    -- We use `take` in `commitStatePatch`, that's why.

    -- History is immutable, we just insert things on top of existing
    -- list.

    -- We don't need to store the whole history in memory, this
    -- structure holds only recent history, the one that will be
    -- inserted to the DB.
  }
  deriving (Generic, Show, Eq)

instance (Serialise s, Serialise p) => Serialise (Archive s p)


type NodeListStory     = NodeStory NgramsState' NgramsStatePatch'

-- NOTE: 'type NgramsTableMap = Map NgramsTerm NgramsRepoElement'
type NgramsState'      = Map       TableNgrams.NgramsType NgramsTableMap
type NgramsStatePatch' = PatchMap  TableNgrams.NgramsType NgramsTablePatch
instance Serialise NgramsStatePatch'
instance FromField (Archive NgramsState' NgramsStatePatch')
  where
    fromField = fromJSONField
instance DefaultFromField SqlJsonb (Archive NgramsState' NgramsStatePatch')
  where
    defaultFromField = fromPGSFromField

-- | Combine `NgramsState'`. This is because the structure is (Map
-- NgramsType (Map ...)) and the default `(<>)` operator is
-- left-biased
-- (https://hackage.haskell.org/package/containers-0.6.6/docs/Data-Map-Internal.html#v:union)
combineState :: NgramsState' -> NgramsState' -> NgramsState'
combineState = Map.unionWith (<>)

-- This is not a typical Semigroup instance. The state is not
-- appended, instead it is replaced with the second entry. This is
-- because state changes with each version. We have to take into
-- account the removal of terms as well.
-- instance (Semigroup s, Semigroup p) => Semigroup (Archive s p) where
--   (<>) (Archive { _a_history = p }) (Archive { _a_version = v'
--                                              , _a_state = s'
--                                              , _a_history = p' }) =
--     Archive { _a_version = v'
--             , _a_state = s'
--             , _a_history = p' <> p }
-- instance (Monoid s, Semigroup p) => Monoid (Archive s p) where
--   mempty = Archive { _a_version = 0
--                    , _a_state = mempty
--                    , _a_history = [] }
instance (FromJSON s, FromJSON p) => FromJSON (Archive s p) where
  parseJSON = genericParseJSON $ unPrefix "_a_"
instance (ToJSON s, ToJSON p) => ToJSON (Archive s p) where
  toJSON     = genericToJSON     $ unPrefix "_a_"
  toEncoding = genericToEncoding $ unPrefix "_a_"

-- | This is the normal way to update archive state, bumping the
-- version and history. Resulting state is taken directly from new
-- archive, omitting old archive completely.
archiveAdvance :: (Semigroup s, Semigroup p) => Archive s p -> Archive s p -> Archive s p
archiveAdvance aOld aNew = aNew { _a_history = _a_history aNew <> _a_history aOld }

-- | This is to merge archive states.
unionArchives :: (Semigroup s, Semigroup p) => Archive s p -> Archive s p -> Archive s p
unionArchives aOld aNew = aNew { _a_state = _a_state aOld <> _a_state aNew
                               , _a_history = _a_history aNew <> _a_history aOld }


------------------------------------------------------------------------
initNodeStory :: (Monoid s, Semigroup p) => NodeId -> NodeStory s p
initNodeStory ni = NodeStory $ Map.singleton ni initArchive

initArchive :: (Monoid s, Semigroup p) => Archive s p
initArchive = Archive { _a_version = 0
                      , _a_state = mempty
                      , _a_history = [] }

initNodeListStoryMock :: NodeListStory
initNodeListStoryMock = NodeStory $ Map.singleton nodeListId archive
  where
    nodeListId = 0
    archive = Archive { _a_version = 0
                      , _a_state = ngramsTableMap
                      , _a_history = [] }
    ngramsTableMap = Map.singleton TableNgrams.NgramsTerms
                   $ Map.fromList
                   [ (n ^. ne_ngrams, ngramsElementToRepo n)
                   | n <- mockTable ^. _NgramsTable
                   ]

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Lenses at the bottom of the file because Template Haskell would reorder order of execution in others cases
makeLenses ''NodeStoryEnv
makeLenses ''NodeStory
makeLenses ''Archive

----------------------------------------------------------------------
data NodeStoryPoly nid v ngtid ngid nre =
  NodeStoryDB { node_id             :: !nid
              , version             :: !v
              , ngrams_type_id      :: !ngtid
              , ngrams_id           :: !ngid
              , ngrams_repo_element :: !nre }
  deriving (Eq)

data NodeStoryArchivePoly nid a =
  NodeStoryArchiveDB { a_node_id :: !nid
                     , archive   :: !a }
  deriving (Eq)

$(makeAdaptorAndInstance "pNodeStory" ''NodeStoryPoly)
$(makeAdaptorAndInstance "pNodeArchiveStory" ''NodeStoryArchivePoly)

-- type NodeStoryWrite = NodeStoryPoly (Column SqlInt4) (Column SqlInt4) (Column SqlInt4) (Column SqlInt4) (Column SqlJsonb)
-- type NodeStoryRead = NodeStoryPoly (Column SqlInt4) (Column SqlInt4) (Column SqlInt4) (Column SqlInt4) (Column SqlJsonb)

-- type NodeStoryArchiveWrite = NodeStoryArchivePoly (Column SqlInt4) (Column SqlJsonb)
-- type NodeStoryArchiveRead = NodeStoryArchivePoly (Column SqlInt4) (Column SqlJsonb)

type ArchiveList = Archive NgramsState' NgramsStatePatch'

-- DB stuff

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
                            :: IO [(Int, TableNgrams.NgramsType, NgramsTerm, NgramsPatch)]

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
                              (nodeId, nType, term, p)) <$> PM.toList patch) <$> PM.toList h :: [(NodeId, TableNgrams.NgramsType, NgramsTerm, NgramsPatch)]
  tuplesM <- mapM (\(nId, nType, term, patch) -> do
                      [PGS.Only ngramsId] <- runPGSReturning c qInsert [PGS.Only term] :: IO [PGS.Only Int]
                      pure (nId, nType, ngramsId, term, patch)
                  ) tuples :: IO [(NodeId, TableNgrams.NgramsType, Int, NgramsTerm, NgramsPatch)]
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

getNodeStory :: PGS.Connection -> NodeId -> IO NodeListStory
getNodeStory c nId = do
  --res <- withResource pool $ \c -> runSelect c query :: IO [NodeStoryPoly NodeId Version Int Int NgramsRepoElement]
  res <- runPGSQuery c nodeStoriesQuery (PGS.Only $ PGS.toField nId) :: IO [(Version, TableNgrams.NgramsType, NgramsTerm, NgramsRepoElement)]
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

  pure $ NodeStory $ Map.singleton nId $ foldl combine initArchive dbData
  where
    -- NOTE (<>) for Archive doesn't concatenate states, so we have to use `combine`
    combine a1 a2 = a1 & a_state %~ combineState (a2 ^. a_state)
                       & a_version .~ (a2 ^. a_version)  -- version should be updated from list, not taken from the empty Archive

nodeStoriesQuery :: PGS.Query
nodeStoriesQuery = [sql| SELECT version, ngrams_type_id, terms, ngrams_repo_element
                           FROM node_stories
                           JOIN ngrams ON ngrams.id = ngrams_id
                           WHERE node_id = ?
                           |]

type ArchiveStateList = [(TableNgrams.NgramsType, NgramsTerm, NgramsRepoElement)]
type ArchiveStateSet = Set.Set (TableNgrams.NgramsType, NgramsTerm)

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

-- nodeStoryRemove :: Pool PGS.Connection -> NodeId -> IO Int64
-- nodeStoryRemove pool (NodeId nId) = withResource pool $ \c -> runDelete c delete
--   where
--     delete = Delete { dTable     = nodeStoryTable
--                     , dWhere     = (\row -> node_id row .== sqlInt4 nId)
--                     , dReturning = rCount }

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

writeNodeStories :: PGS.Connection -> NodeListStory -> IO ()
writeNodeStories c (NodeStory nls) = do
  mapM_ (\(nId, a) -> upsertNodeStories c nId a) $ Map.toList nls

-- | Returns a `NodeListStory`, updating the given one for given `NodeId`
nodeStoryInc :: PGS.Connection -> NodeListStory -> NodeId -> IO NodeListStory
nodeStoryInc c ns@(NodeStory nls) nId = do
  case Map.lookup nId nls of
    Nothing -> do
      NodeStory nls' <- getNodeStory c nId
      pure $ NodeStory $ Map.unionWith archiveAdvance nls' nls
    Just _ -> pure ns

nodeStoryIncrementalRead :: PGS.Connection -> Maybe NodeListStory -> [NodeId] -> IO NodeListStory
nodeStoryIncrementalRead _ Nothing [] = pure $ NodeStory $ Map.empty
nodeStoryIncrementalRead c Nothing (ni:ns) = do
  m <- getNodeStory c ni
  nodeStoryIncrementalRead c (Just m) ns
nodeStoryIncrementalRead c (Just nls) ns = foldM (\m n -> nodeStoryInc c m n) nls ns

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

-- | NgramsRepoElement contains, in particular, `nre_list`,
-- `nre_parent` and `nre_children`. We want to make sure that all
-- children entries (i.e. ones that have `nre_parent`) have the same
-- `list` as their parent entry.
fixChildrenTermTypes :: NodeListStory -> NodeListStory
fixChildrenTermTypes (NodeStory nls) =
  NodeStory $ Map.fromList [ (nId, a & a_state %~ fixChildrenInNgramsStatePatch)
                           | (nId, a) <- Map.toList nls ]

fixChildrenInNgramsStatePatch :: NgramsState' -> NgramsState'
fixChildrenInNgramsStatePatch ns = archiveStateFromList $ nsParents <> nsChildrenFixed
  where
    nls = archiveStateToList ns
    
    nsParents = filter (\(_nt, _t, nre) -> isNothing $ nre ^. nre_parent) nls
    parentNtMap = Map.fromList $ (\(_nt, t, nre) -> (t, nre ^. nre_list)) <$> nsParents
    
    nsChildren = filter (\(_nt, _t, nre) -> isJust $ nre ^. nre_parent) nls
    nsChildrenFixed = (\(nt, t, nre) ->
                         ( nt
                         , t
                         , nre & nre_list %~
                            (\l -> parentNtMap ^. at (nre ^. nre_parent . _Just) . non l)
                         )
                      ) <$> nsChildren

-- | Sometimes, when we upload a new list, a child can be left without
-- a parent. Find such ngrams and set their 'root' and 'parent' to
-- 'Nothing'.
fixChildrenWithNoParent :: NodeListStory -> NodeListStory
fixChildrenWithNoParent (NodeStory nls) =
  NodeStory $ Map.fromList [ (nId, a & a_state %~ fixChildrenWithNoParentStatePatch)
                           | (nId, a) <- Map.toList nls ]

fixChildrenWithNoParentStatePatch :: NgramsState' -> NgramsState'
fixChildrenWithNoParentStatePatch ns = archiveStateFromList $ nsParents <> nsChildrenFixed
  where
    nls = archiveStateToList ns
    
    nsParents = filter (\(_nt, _t, nre) -> isNothing $ nre ^. nre_parent) nls
    parentNtMap = Map.fromList $ (\(_nt, t, nre) -> (t, nre ^. nre_children & mSetToSet)) <$> nsParents
    
    nsChildren = filter (\(_nt, _t, nre) -> isJust $ nre ^. nre_parent) nls
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

------------------------------------

fromDBNodeStoryEnv :: Pool PGS.Connection -> IO NodeStoryEnv
fromDBNodeStoryEnv pool = do
  tvar <- nodeStoryVar pool Nothing []
  let saver_immediate = do
        ns <- atomically $
              readTVar tvar
              -- fix children so their 'list' is the same as their parents'
              >>= pure . fixChildrenTermTypes
              -- fix children that don't have a parent anymore
              >>= pure . fixChildrenWithNoParent
              >>= writeTVar tvar
              >> readTVar tvar
        withResource pool $ \c -> do
          --printDebug "[mkNodeStorySaver] will call writeNodeStories, ns" ns
          writeNodeStories c ns
  let archive_saver_immediate ns@(NodeStory nls) = withResource pool $ \c -> do
        mapM_ (\(nId, a) -> do
                  insertNodeArchiveHistory c nId (a ^. a_version) $ reverse $ a ^. a_history
              ) $ Map.toList nls
        pure $ clearHistory ns
        
  pure $ NodeStoryEnv { _nse_var    = tvar
                      , _nse_saver_immediate = saver_immediate
                      , _nse_archive_saver_immediate = archive_saver_immediate
                      , _nse_getter = nodeStoryVar pool (Just tvar)
                      }

nodeStoryVar :: Pool PGS.Connection
             -> Maybe (TVar NodeListStory)
             -> [NodeId]
             -> IO (TVar NodeListStory)
nodeStoryVar pool Nothing nIds = do
  state' <- withResource pool $ \c -> nodeStoryIncrementalRead c Nothing nIds
  atomically $ newTVar state'
nodeStoryVar pool (Just tv) nIds = do
  nls <- atomically $ readTVar tv
  nls' <- withResource pool
      $ \c -> nodeStoryIncrementalRead c (Just nls) nIds
  _ <- atomically $ writeTVar tv nls'
  pure tv

clearHistory :: NodeListStory -> NodeListStory
clearHistory (NodeStory ns) = NodeStory $ ns & (traverse . a_history) .~ emptyHistory
  where
    emptyHistory = [] :: [NgramsStatePatch']

currentVersion :: (HasNodeStory env err m) => ListId -> m Version
currentVersion listId = do
  pool <- view connPool
  nls <- withResource pool $ \c -> liftBase $ getNodeStory c listId
  pure $ nls ^. unNodeStory . at listId . _Just . a_version


-----------------------------------------

-- | To be called from the REPL
fixNodeStoryVersions :: (HasNodeStory env err m) => m ()
fixNodeStoryVersions = do
  pool <- view connPool
  _ <- withResource pool $ \c -> liftBase $ PGS.withTransaction c $ do
    nIds <- runPGSQuery c [sql| SELECT id FROM nodes WHERE ? |] (PGS.Only True) :: IO [PGS.Only Int64]
    -- printDebug "[fixNodeStoryVersions] nIds" nIds
    mapM_ (\(PGS.Only nId) -> do
        -- printDebug "[fixNodeStoryVersions] nId" nId
        updateVer c TableNgrams.Authors nId

        updateVer c TableNgrams.Institutes nId

        updateVer c TableNgrams.Sources nId

        updateVer c TableNgrams.NgramsTerms nId

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
    updateVer :: PGS.Connection -> TableNgrams.NgramsType -> Int64 -> IO ()
    updateVer c ngramsType nId = do
      maxVer <- runPGSQuery c maxVerQuery (nId, ngramsType) :: IO [PGS.Only (Maybe Int64)]
      case maxVer of
        [] -> pure ()
        [PGS.Only Nothing] -> pure ()
        [PGS.Only (Just maxVersion)] -> do
          _ <- runPGSExecute c updateVerQuery (maxVersion, nId, ngramsType)
          pure ()
        _ -> panicTrace "Should get only 1 result!"
