{-|
Module      : Gargantext.API.Ngrams
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams API

-- | TODO
get ngrams filtered by NgramsType
add get

-}

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}

{-# LANGUAGE IncoherentInstances #-}

module Gargantext.API.Ngrams
  ( TableNgramsApi
  , TableNgramsApiGet
  , TableNgramsApiPut

  , commitStatePatch
  
  , searchTableNgrams
  , getTableNgrams
  , getTableNgramsCorpus
  , setListNgrams
  --, rmListNgrams TODO fix before exporting
  , apiNgramsTableCorpus
  , apiNgramsTableDoc

  , NgramsTablePatch
  , NgramsTableMap

  , NgramsTerm(..)

  , NgramsElement(..)
  , mkNgramsElement

  , RootParent(..)

  , MSet
  , mSetFromList
  , mSetToList

  , Repo(..)
  , r_version
  , r_state
  , r_history
  , NgramsRepoElement(..)
  , saveNodeStory
  , saveNodeStoryImmediate
  , initRepo

  , TabType(..)

  , QueryParamR
  , TODO

  -- Internals
  , getNgramsTableMap
  , dumpJsonTableMap
  , tableNgramsPull
  , tableNgramsPut

  , getNgramsTable'
  , setNgramsTableScores

  , Version
  , Versioned(..)
  , VersionedWithCount(..)
  , currentVersion
  , listNgramsChangedSince
  , MinSize, MaxSize, OrderBy, NgramsTable
  , UpdateTableNgramsCharts
  )
  where

import Control.Lens ((.~), view, (^.), (^..), (+~), (%~), (.~), msumOf, at, _Just, Each(..), (%%~), mapped, ifolded, to, withIndex, over)
import Control.Monad.Reader
import Data.Aeson.Text qualified as DAT
import Data.Foldable
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Map.Strict.Patch qualified as PM
import Data.Monoid
import Data.Patch.Class (Action(act), Transformable(..), ours)
import Data.Set qualified as Set
import Data.Text (isInfixOf, toLower, unpack)
import Data.Text.Lazy.IO as DTL
import Formatting (hprint, int, (%))
import Gargantext.API.Admin.EnvTypes (Env, GargJob(..))
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Metrics qualified as Metrics
import Gargantext.API.Ngrams.Tools
import Gargantext.API.Ngrams.Types
import Gargantext.API.Prelude
import Gargantext.Core.NodeStory
import Gargantext.Core.Types (ListType(..), NodeId, ListId, DocId, TODO, assertValid, HasInvalidError, ContextId)
import Gargantext.Core.Types.Query (Limit(..), Offset(..), MinSize(..), MaxSize(..))
import Gargantext.Database.Action.Metrics.NgramsByContext (getOccByNgramsOnlyFast)
import Gargantext.Database.Admin.Config (userMaster)
import Gargantext.Database.Admin.Types.Node (NodeType(..))
import Gargantext.Database.Query.Table.Ngrams hiding (NgramsType(..), ngramsType, ngrams_terms)
import Gargantext.Database.Query.Table.Ngrams qualified as TableNgrams
import Gargantext.Database.Query.Table.Node (getNode)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.Node.Select
import Gargantext.Database.Schema.Node (node_id, node_parent_id, node_user_id)
import Gargantext.Prelude hiding (log, to, toLower, (%))
import Gargantext.Prelude.Clock (hasTime, getTime)
import Gargantext.Utils.Jobs (serveJobsAPI, MonadJobStatus(..))
import GHC.Conc (readTVar, writeTVar)
import Prelude (error)
import Servant hiding (Patch)

{-
-- TODO sequences of modifications (Patchs)
type NgramsIdPatch = Patch NgramsId NgramsPatch

ngramsPatch :: Int -> NgramsPatch
ngramsPatch n = NgramsPatch (DM.fromList [(1, StopTerm)]) (Set.fromList [n]) Set.empty

toEdit :: NgramsId -> NgramsPatch -> Edit NgramsId NgramsPatch
toEdit n p = Edit n p
ngramsIdPatch :: Patch NgramsId NgramsPatch
ngramsIdPatch = fromList $ catMaybes $ reverse [ replace (1::NgramsId) (Just $ ngramsPatch 1) Nothing
                                       , replace (1::NgramsId) Nothing (Just $ ngramsPatch 2)
                                       , replace (2::NgramsId) Nothing (Just $ ngramsPatch 2)
                                       ]

-- applyPatchBack :: Patch -> IO Patch
-- isEmptyPatch = Map.all (\x -> Set.isEmpty (add_children x) && Set.isEmpty ... )
-}
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------

{-
-- TODO: Replace.old is ignored which means that if the current list
-- `MapTerm` and that the patch is `Replace CandidateTerm StopTerm` then
-- the list is going to be `StopTerm` while it should keep `MapTerm`.
-- However this should not happen in non conflicting situations.
mkListsUpdate :: NgramsType -> NgramsTablePatch -> [(NgramsTypeId, NgramsTerm, ListTypeId)]
mkListsUpdate nt patches =
  [ (ngramsTypeId nt, ng, listTypeId lt)
  | (ng, patch) <- patches ^.. ntp_ngrams_patches . ifolded . withIndex
  , lt <- patch ^.. patch_list . new
  ]

mkChildrenGroups :: (PatchSet NgramsTerm -> Set NgramsTerm)
                 -> NgramsType
                 -> NgramsTablePatch
                 -> [(NgramsTypeId, NgramsParent, NgramsChild)]
mkChildrenGroups addOrRem nt patches =
  [ (ngramsTypeId nt, parent, child)
  | (parent, patch) <- patches ^.. ntp_ngrams_patches . ifolded . withIndex
  , child <- patch ^.. patch_children . to addOrRem . folded
  ]
-}

------------------------------------------------------------------------

saveNodeStory :: ( MonadReader env m, MonadBase IO m, HasNodeStorySaver env )
              => m ()
saveNodeStory = do
  saver <- view hasNodeStorySaver
  liftBase $ do
    --Gargantext.Prelude.putStrLn "---- Running node story saver ----"
    saver
    --Gargantext.Prelude.putStrLn "---- Node story saver finished ----"


saveNodeStoryImmediate :: ( MonadReader env m, MonadBase IO m, HasNodeStoryImmediateSaver env )
                       => m ()
saveNodeStoryImmediate = do
  saver <- view hasNodeStoryImmediateSaver
  liftBase $ do
    --Gargantext.Prelude.putStrLn "---- Running node story immediate saver ----"
    saver
    --Gargantext.Prelude.putStrLn "---- Node story immediate saver finished ----"

listTypeConflictResolution :: ListType -> ListType -> ListType
listTypeConflictResolution _ _ = undefined -- TODO Use Map User ListType


ngramsStatePatchConflictResolution :: TableNgrams.NgramsType
                                   -> NgramsTerm
                                   -> ConflictResolutionNgramsPatch
ngramsStatePatchConflictResolution _ngramsType _ngramsTerm
   = (ours, (const ours, ours), (False, False))
                             -- (False, False) mean here that Mod has always priority.
 -- = (ours, (const ours, ours), (True, False))
                             -- (True, False) <- would mean priority to the left (same as ours).
  -- undefined {- TODO think this through -}, listTypeConflictResolution)




-- Current state:
--   Insertions are not considered as patches,
--   they do not extend history,
--   they do not bump version.
insertNewOnly :: a -> Maybe b -> a
insertNewOnly m = maybe m (const $ error "insertNewOnly: impossible")
  -- TODO error handling

{- unused
-- TODO refactor with putListNgrams
copyListNgrams :: RepoCmdM env err m
               => NodeId -> NodeId -> NgramsType
               -> m ()
copyListNgrams srcListId dstListId ngramsType = do
  var <- view repoVar
  liftBase $ modifyMVar_ var $
    pure . (r_state . at ngramsType %~ (Just . f . something))
  saveNodeStory
  where
    f :: Map NodeId NgramsTableMap -> Map NodeId NgramsTableMap
    f m = m & at dstListId %~ insertNewOnly (m ^. at srcListId)

-- TODO refactor with putListNgrams
-- The list must be non-empty!
-- The added ngrams must be non-existent!
addListNgrams :: RepoCmdM env err m
              => NodeId -> NgramsType
              -> [NgramsElement] -> m ()
addListNgrams listId ngramsType nes = do
  var <- view repoVar
  liftBase $ modifyMVar_ var $
    pure . (r_state . at ngramsType . _Just . at listId . _Just <>~ m)
  saveNodeStory
  where
    m = Map.fromList $ (\n -> (n ^. ne_ngrams, n)) <$> nes
-}

-- | TODO: incr the Version number
-- && should use patch
-- UNSAFE
setListNgrams :: HasNodeStory env err m
              => NodeId
              -> TableNgrams.NgramsType
              -> Map NgramsTerm NgramsRepoElement
              -> m ()
setListNgrams listId ngramsType ns = do
  -- printDebug "[setListNgrams]" (listId, ngramsType)
  var <- getNodeStoryVar [listId]
  liftBase $ atomically $ do
    nls <- readTVar var
    writeTVar var $
      ( unNodeStory
        . at listId . _Just
        . a_state
        . at ngramsType
        .~ Just ns
      ) nls
  saveNodeStory


newNgramsFromNgramsStatePatch :: NgramsStatePatch' -> [Ngrams]
newNgramsFromNgramsStatePatch p =
  [ text2ngrams (unNgramsTerm n)
  | (n,np) <- p ^.. _PatchMap
                -- . each . _PatchMap
                . each . _NgramsTablePatch
                . _PatchMap . ifolded . withIndex
  , _ <- np ^.. patch_new . _Just
  ]




commitStatePatch :: ( HasNodeStory env err m
                    , HasNodeStoryImmediateSaver env
                    , HasNodeArchiveStoryImmediateSaver env )
                 => ListId
                 ->    Versioned NgramsStatePatch'
                 -> m (Versioned NgramsStatePatch')
commitStatePatch listId (Versioned _p_version p) = do
  -- printDebug "[commitStatePatch]" listId
  var <- getNodeStoryVar [listId]
  archiveSaver <- view hasNodeArchiveStoryImmediateSaver
  ns <- liftBase $ atomically $ readTVar var
  let
    a = ns ^. unNodeStory . at listId . _Just
    -- apply patches from version p_version to a ^. a_version
    -- TODO Check this
    --q = mconcat $ take (a ^. a_version - p_version) (a ^. a_history)
    q = mconcat $ a ^. a_history

  --printDebug "[commitStatePatch] transformWith" (p,q)
  -- let tws s = case s of
  --       (Mod p) -> "Mod"
  --       _ -> "Rpl"
  -- printDebug "[commitStatePatch] transformWith" (tws $ p ^. _NgramsPatch, tws $ q ^. _NgramsPatch)

  let
    (p', q') = transformWith ngramsStatePatchConflictResolution p q
    a' = a & a_version +~ 1
           & a_state   %~ act p'
           & a_history %~ (p' :)

  {-
  -- Ideally we would like to check these properties. However:
  -- * They should be checked only to debug the code. The client data
  --   should be able to trigger these.
  -- * What kind of error should they throw (we are in IO here)?
  -- * Should we keep modifyMVar?
  -- * Should we throw the validation in an Exception, catch it around
  --   modifyMVar and throw it back as an Error?
  assertValid $ transformable p q
  assertValid $ applicable p' (r ^. r_state)
  -}
  -- printDebug "[commitStatePatch] a version" (a ^. a_version)
  -- printDebug "[commitStatePatch] a' version" (a' ^. a_version)
  let newNs = ( ns & unNodeStory . at listId .~ (Just a')
       , Versioned (a' ^. a_version) q'
       )

  -- NOTE Now is the only good time to save the archive history. We
  -- have the handle to the MVar and we need to save its exact
  -- snapshot. Node Story archive is a linear table, so it's only
  -- couple of inserts, it shouldn't take long...

  -- NOTE This is changed now. Before we used MVar's, now it's TVars
  -- (MVar's blocked). It was wrapped in withMVar before, now we read
  -- the TVar, modify archive with archiveSaver, then write the tvar.

  -- pure (newNs', snd newNs)
  -- writeTVar var newNs'

  --pure newNs

  -- If we postponed saving the archive to the debounce action, we
  -- would have issues like
  -- https://gitlab.iscpif.fr/gargantext/purescript-gargantext/issues/476
  -- where the `q` computation from above (which uses the archive)
  -- would cause incorrect patch application (before the previous
  -- archive was saved and applied)
  -- newNs' <- archiveSaver $ fst newNs
  liftBase $ do
    newNs' <- archiveSaver $ fst newNs
    atomically $ writeTVar var newNs'

  -- Save new ngrams
  _ <- insertNgrams (newNgramsFromNgramsStatePatch p)
  -- NOTE State (i.e. `NodeStory` can be saved asynchronously, i.e. with debounce)
  -- saveNodeStory
  saveNodeStoryImmediate

  pure $ snd newNs



-- This is a special case of tableNgramsPut where the input patch is empty.
tableNgramsPull :: HasNodeStory env err m
                => ListId
                -> TableNgrams.NgramsType
                -> Version
                -> m (Versioned NgramsTablePatch)
tableNgramsPull listId ngramsType p_version = do
  -- printDebug "[tableNgramsPull]" (listId, ngramsType)
  var <- getNodeStoryVar [listId]
  r <- liftBase $ atomically $ readTVar var

  let
    a = r ^. unNodeStory . at listId . _Just
    q = mconcat $ take (a ^. a_version - p_version) (a ^. a_history)
    q_table = q ^. _PatchMap . at ngramsType . _Just

  pure (Versioned (a ^. a_version) q_table)




-- tableNgramsPut :: (HasInvalidError err, RepoCmdM env err m)
-- Apply the given patch to the DB and returns the patch to be applied on the
-- client.
-- TODO-ACCESS check
tableNgramsPut :: ( HasNodeStory    env err m
                  , HasNodeStoryImmediateSaver env
                  , HasNodeArchiveStoryImmediateSaver env
                  , HasInvalidError     err
                  )
                 => TabType
                 -> ListId
                 -> Versioned NgramsTablePatch
                 -> m (Versioned NgramsTablePatch)
tableNgramsPut tabType listId (Versioned p_version p_table)
  | p_table == mempty = do
      -- printDebug "[tableNgramsPut]" ("TableEmpty" :: Text)
      let ngramsType        = ngramsTypeFromTabType tabType
      tableNgramsPull listId ngramsType p_version

  | otherwise         = do
      -- printDebug "[tableNgramsPut]" ("TableNonEmpty" :: Text)
      let ngramsType        = ngramsTypeFromTabType tabType
          (p, p_validity)   = PM.singleton ngramsType p_table

      assertValid p_validity

      ret <- commitStatePatch listId (Versioned p_version p)
        <&> v_data %~ (view (_PatchMap . at ngramsType . _Just))

      pure ret



tableNgramsPostChartsAsync :: ( HasNodeStory env err m
                              , HasSettings env
                              , MonadJobStatus m )
                            => UpdateTableNgramsCharts
                            -> JobHandle m
                            -> m ()
tableNgramsPostChartsAsync utn jobHandle = do
      let tabType = utn ^. utn_tab_type
      let listId = utn ^. utn_list_id

      node <- getNode listId
      let _nId = node ^. node_id
          _uId = node ^. node_user_id
          mCId = node ^. node_parent_id

      -- printDebug "[tableNgramsPostChartsAsync] tabType" tabType
      -- printDebug "[tableNgramsPostChartsAsync] listId" listId

      case mCId of
        Nothing -> do
          -- printDebug "[tableNgramsPostChartsAsync] can't update charts, no parent, nId" nId
          markStarted 1 jobHandle
          markFailed Nothing jobHandle
        Just cId -> do
          case tabType of
            Authors -> do
              -- printDebug "[tableNgramsPostChartsAsync] Authors, updating Pie, cId" cId
              markStarted 1 jobHandle
              _ <- Metrics.updatePie cId (Just listId) tabType Nothing
              markComplete jobHandle
            Institutes -> do
              -- printDebug "[tableNgramsPostChartsAsync] Institutes, updating Tree, cId" cId
              -- printDebug "[tableNgramsPostChartsAsync] updating tree StopTerm, cId" cId
              markStarted 3 jobHandle
              _ <- Metrics.updateTree cId (Just listId) tabType StopTerm
              -- printDebug "[tableNgramsPostChartsAsync] updating tree CandidateTerm, cId" cId
              markProgress 1 jobHandle
              _ <- Metrics.updateTree cId (Just listId) tabType CandidateTerm
              -- printDebug "[tableNgramsPostChartsAsync] updating tree MapTerm, cId" cId
              markProgress 1 jobHandle
              _ <- Metrics.updateTree cId (Just listId) tabType MapTerm
              markComplete jobHandle
            Sources -> do
              -- printDebug "[tableNgramsPostChartsAsync] Sources, updating chart, cId" cId
              markStarted 1 jobHandle
              _ <- Metrics.updatePie cId (Just listId) tabType Nothing
              markComplete jobHandle
            Terms -> do
              -- printDebug "[tableNgramsPostChartsAsync] Terms, updating Metrics (Histo), cId" cId
              markStarted 6 jobHandle
{-
              _ <- Metrics.updateChart cId listId tabType Nothing
              logRefSuccess
              _ <- Metrics.updatePie cId (Just listId) tabType Nothing
              logRefSuccess
              _ <- Metrics.updateScatter cId (Just listId) tabType Nothing
              logRefSuccess
              _ <- Metrics.updateTree cId (Just listId) tabType StopTerm
              logRefSuccess
              _ <- Metrics.updateTree cId (Just listId) tabType CandidateTerm
              logRefSuccess
              _ <- Metrics.updateTree cId (Just listId) tabType MapTerm
-}
              markComplete jobHandle
            _ -> do
              -- printDebug "[tableNgramsPostChartsAsync] no update for tabType = " tabType
              markStarted 1 jobHandle
              markFailed Nothing jobHandle

  {-
  { _ne_list        :: ListType
  If we merge the parents/children we can potentially create cycles!
  , _ne_parent      :: Maybe NgramsTerm
  , _ne_children    :: MSet NgramsTerm
  }
  -}

getNgramsTableMap :: HasNodeStory env err m
                  => NodeId
                  -> TableNgrams.NgramsType
                  -> m (Versioned NgramsTableMap)
getNgramsTableMap nodeId ngramsType = do
  v    <- getNodeStoryVar [nodeId]
  repo <- liftBase $ atomically $ readTVar v
  pure $ Versioned (repo ^. unNodeStory . at nodeId . _Just . a_version)
                   (repo ^. unNodeStory . at nodeId . _Just . a_state . at ngramsType . _Just)


dumpJsonTableMap :: HasNodeStory env err m
                 => Text
                 -> NodeId
                 -> TableNgrams.NgramsType
                 -> m ()
dumpJsonTableMap fpath nodeId ngramsType = do
  m <- getNgramsTableMap nodeId ngramsType
  liftBase $ DTL.writeFile (unpack fpath) (DAT.encodeToLazyText m)
  pure ()


-- | TODO Errors management
--  TODO: polymorphic for Annuaire or Corpus or ...
-- | Table of Ngrams is a ListNgrams formatted (sorted and/or cut).
-- TODO: should take only one ListId

-- | /pure/ function to query a 'Map NgramsTerm NgramsElement', according to a
-- search function. Returns a /versioned/ 'NgramsTable' which is paginated and
-- sorted according to the input 'NgramsSearchQuery', together with the
-- occurrences of the elements.
searchTableNgrams :: Versioned (Map NgramsTerm NgramsElement)
                  -> NgramsSearchQuery
                  -- ^ The search query on the retrieved data
                  -> VersionedWithCount NgramsTable
searchTableNgrams versionedTableMap NgramsSearchQuery{..} =
  let tableMap     = versionedTableMap ^. v_data
      filteredData = filterNodes tableMap
      tableMapSorted = versionedTableMap
                     & v_data .~ (NgramsTable . sortAndPaginate . withInners tableMap $ filteredData)

  in toVersionedWithCount (Set.size filteredData) tableMapSorted
  where

    -- | Returns the \"root\" of the 'NgramsElement', or it falls back to the input
    -- 'NgramsElement' itself, if no root can be found.
    -- /CAREFUL/: The root we select might /not/ have the same 'listType' we are
    -- filtering for, in which case we have to change its type to match, if needed.
    rootOf :: Map NgramsTerm NgramsElement -> NgramsElement -> NgramsElement
    rootOf tblMap ne = case ne ^. ne_root of
      Nothing -> ne
      Just rootKey
        | Just r <- tblMap ^. at rootKey
        -- NOTE(adinapoli) It's unclear what is the correct behaviour here: should
        -- we override the type or we filter out the node altogether?
        -> over ne_list (\oldList -> fromMaybe oldList _nsq_listType) r
        | otherwise
        -> ne

    -- | Returns 'True' if the input 'NgramsElement' satisfies the search criteria
    -- mandated by 'NgramsSearchQuery'.
    matchingNode :: NgramsElement -> Bool
    matchingNode inputNode =
      let nodeSize        = inputNode ^. ne_size
          matchesListType = maybe (const True) (==) _nsq_listType
          respectsMinSize = maybe (const True) (<=) (getMinSize <$> _nsq_minSize)
          respectsMaxSize = maybe (const True) (>=) (getMaxSize <$> _nsq_maxSize)

      in    respectsMinSize nodeSize
         && respectsMaxSize nodeSize
         && _nsq_searchQuery (inputNode ^. ne_ngrams)
         && matchesListType (inputNode ^. ne_list)

    sortOnOrder :: Maybe OrderBy -> ([NgramsElement] -> [NgramsElement])
    sortOnOrder Nothing          = sortOnOrder (Just ScoreDesc)
    sortOnOrder (Just TermAsc)   = List.sortOn $ view ne_ngrams
    sortOnOrder (Just TermDesc)  = List.sortOn $ Down . view ne_ngrams
    sortOnOrder (Just ScoreAsc)  = List.sortOn $ view (ne_occurrences . to Set.size)
    sortOnOrder (Just ScoreDesc) = List.sortOn $ Down . view (ne_occurrences . to Set.size)

    -- | Filters the given `tableMap` with the search criteria. It returns
    -- a set of 'NgramsElement' all matching the input 'NGramsSearchQuery'.
    filterNodes :: Map NgramsTerm NgramsElement -> Set NgramsElement
    filterNodes tblMap = Set.map (rootOf tblMap) selectedNodes
      where
        allNodes      = Set.fromList $ Map.elems tblMap
        selectedNodes = Set.filter matchingNode allNodes

    -- | For each input root, extends its occurrence count with
    -- the information found in the subitems.
    withInners :: Map NgramsTerm NgramsElement -> Set NgramsElement -> Set NgramsElement
    withInners tblMap roots = Set.map addSubitemsOccurrences roots
      where
        addSubitemsOccurrences :: NgramsElement -> NgramsElement
        addSubitemsOccurrences e =
          e { _ne_occurrences = foldl' alterOccurrences (e ^. ne_occurrences) (e ^. ne_children) }

        alterOccurrences :: Set ContextId -> NgramsTerm -> Set ContextId
        alterOccurrences occs t = case Map.lookup t tblMap of
          Nothing -> occs
          Just e' -> occs <> e' ^. ne_occurrences

    -- | Paginate the results
    sortAndPaginate :: Set NgramsElement -> [NgramsElement]
    sortAndPaginate xs =
      let offset'  = getOffset $ maybe 0 identity _nsq_offset
      in   take (getLimit _nsq_limit)
         . drop offset'
         . sortOnOrder _nsq_orderBy
         . Set.toList
         $ xs


getTableNgrams :: forall env err m.
                  ( HasNodeStory env err m
                  , HasNodeError err )
               => NodeId
               -> ListId
               -> TabType
               -> NgramsSearchQuery
               -> m (VersionedWithCount NgramsTable)
getTableNgrams nodeId listId tabType searchQuery = do
  let ngramsType = ngramsTypeFromTabType tabType
  versionedInput <- getNgramsTable' nodeId listId ngramsType
  pure $ searchTableNgrams versionedInput searchQuery


-- | Helper function to get the ngrams table with scores.
getNgramsTable' :: forall env err m.
                   ( HasNodeStory env err m
                   , HasNodeError err )
                => NodeId
                -> ListId
                -> TableNgrams.NgramsType
                -> m (Versioned (Map.Map NgramsTerm NgramsElement))
getNgramsTable' nId listId ngramsType = do
  tableMap <- getNgramsTableMap listId ngramsType
  tableMap & v_data %%~ (setNgramsTableScores nId listId ngramsType)
                        . Map.mapWithKey ngramsElementFromRepo

-- | Helper function to set scores on an `NgramsTable`.
setNgramsTableScores :: forall env err m t.
                        ( Each t t NgramsElement NgramsElement
                        , HasNodeStory env err m
                        , HasNodeError err )
                     => NodeId
                     -> ListId
                     -> TableNgrams.NgramsType
                     -> t
                     -> m t
setNgramsTableScores nId listId ngramsType table = do
  t1 <- getTime
  occurrences <- getOccByNgramsOnlyFast nId listId ngramsType
  --printDebug "[setNgramsTableScores] occurrences" occurrences
  t2 <- getTime
  liftBase $ do
    let ngrams_terms = table ^.. each . ne_ngrams
    -- printDebug "ngrams_terms" ngrams_terms
    hprint stderr
      ("getTableNgrams/setScores #ngrams=" % int % " time=" % hasTime % "\n")
      (length ngrams_terms) t1 t2
  let
    setOcc ne = ne & ne_occurrences .~ Set.fromList (msumOf (at (ne ^. ne_ngrams) . _Just) occurrences)

  --printDebug "[setNgramsTableScores] with occurences" $ table & each %~ setOcc

  pure $ table & each %~ setOcc




scoresRecomputeTableNgrams :: forall env err m.
                              ( HasNodeStory env err m, HasNodeError err )
                           => NodeId -> TabType -> ListId -> m Int
scoresRecomputeTableNgrams nId tabType listId = do
  tableMap <- getNgramsTableMap listId ngramsType
  _ <- tableMap & v_data %%~ (setNgramsTableScores nId listId ngramsType)
                           . Map.mapWithKey ngramsElementFromRepo

  pure $ 1
  where
    ngramsType = ngramsTypeFromTabType tabType


-- APIs

-- TODO: find a better place for the code above, All APIs stay here

needsScores :: Maybe OrderBy -> Bool
needsScores (Just ScoreAsc)  = True
needsScores (Just ScoreDesc) = True
needsScores _ = False

type TableNgramsApiGet = Summary " Table Ngrams API Get"
                      :> QueryParamR "ngramsType"  TabType
                      :> QueryParamR "list"        ListId
                      :> QueryParamR "limit"       Limit
                      :> QueryParam  "offset"      Offset
                      :> QueryParam  "listType"    ListType
                      :> QueryParam  "minTermSize" MinSize
                      :> QueryParam  "maxTermSize" MaxSize
                      :> QueryParam  "orderBy"     OrderBy
                      :> QueryParam  "search"      Text
                      :> Get    '[JSON] (VersionedWithCount NgramsTable)

type TableNgramsApiPut = Summary " Table Ngrams API Change"
                       :> QueryParamR "ngramsType" TabType
                       :> QueryParamR "list"       ListId
                       :> ReqBody '[JSON] (Versioned NgramsTablePatch)
                       :> Put     '[JSON] (Versioned NgramsTablePatch)

type RecomputeScoresNgramsApiGet = Summary " Recompute scores for ngrams table"
                       :> QueryParamR "ngramsType"  TabType
                       :> QueryParamR "list"        ListId
                       :> "recompute" :> Post '[JSON] Int

type TableNgramsApiGetVersion = Summary " Table Ngrams API Get Version"
                      :> QueryParamR "ngramsType"  TabType
                      :> QueryParamR "list"        ListId
                      :> Get    '[JSON] Version

type TableNgramsApi =  TableNgramsApiGet
                  :<|> TableNgramsApiPut
                  :<|> RecomputeScoresNgramsApiGet
                  :<|> "version" :> TableNgramsApiGetVersion
                  :<|> TableNgramsAsyncApi

type TableNgramsAsyncApi = Summary "Table Ngrams Async API"
                           :> "async"
                           :> "charts"
                           :> "update"
                           :> AsyncJobs JobLog '[JSON] UpdateTableNgramsCharts JobLog

getTableNgramsCorpus :: ( HasNodeStory env err m
                        , HasNodeError err )
                     => NodeId
                     -> TabType
                     -> ListId
                     -> Limit
                     -> Maybe Offset
                     -> Maybe ListType
                     -> Maybe MinSize -> Maybe MaxSize
                     -> Maybe OrderBy
                     -> Maybe Text -- full text search
                     -> m (VersionedWithCount NgramsTable)
getTableNgramsCorpus nId tabType listId limit_ offset listType minSize maxSize orderBy mt =
  getTableNgrams nId listId tabType searchQuery
    where
      searchQueryFn (NgramsTerm nt) = maybe (const True) isInfixOf (toLower <$> mt) (toLower nt)
      searchQuery = NgramsSearchQuery {
                    _nsq_limit       = limit_
                  , _nsq_offset      = offset
                  , _nsq_listType    = listType
                  , _nsq_minSize     = minSize
                  , _nsq_maxSize     = maxSize
                  , _nsq_orderBy     = orderBy
                  , _nsq_searchQuery = searchQueryFn
                  }




getTableNgramsVersion :: ( HasNodeStory env err m
                         , HasNodeError err )
                      => NodeId
                      -> TabType
                      -> ListId
                      -> m Version
getTableNgramsVersion _nId _tabType listId = currentVersion listId



  -- TODO: limit?
  -- Versioned { _v_version = v } <- getTableNgramsCorpus nId tabType listId 100000 Nothing Nothing Nothing Nothing Nothing Nothing
  -- This line above looks like a waste of computation to finally get only the version.
  -- See the comment about listNgramsChangedSince.


-- | Text search is deactivated for now for ngrams by doc only
getTableNgramsDoc :: ( HasNodeStory env err m
                     , HasNodeError err )
                  => DocId -> TabType
                  -> ListId -> Limit -> Maybe Offset
                  -> Maybe ListType
                  -> Maybe MinSize -> Maybe MaxSize
                  -> Maybe OrderBy
                  -> Maybe Text -- full text search
                  -> m (VersionedWithCount NgramsTable)
getTableNgramsDoc dId tabType listId limit_ offset listType minSize maxSize orderBy _mt = do
  ns <- selectNodesWithUsername NodeList userMaster
  let ngramsType = ngramsTypeFromTabType tabType
  ngs <- selectNgramsByDoc (ns <> [listId]) dId ngramsType
  let searchQueryFn (NgramsTerm nt) = flip Set.member (Set.fromList ngs) nt
      searchQuery = NgramsSearchQuery {
                    _nsq_limit       = limit_
                  , _nsq_offset      = offset
                  , _nsq_listType    = listType
                  , _nsq_minSize     = minSize
                  , _nsq_maxSize     = maxSize
                  , _nsq_orderBy     = orderBy
                  , _nsq_searchQuery = searchQueryFn
                  }
  getTableNgrams dId listId tabType searchQuery


apiNgramsTableCorpus :: NodeId -> ServerT TableNgramsApi (GargM Env GargError)
apiNgramsTableCorpus cId =  getTableNgramsCorpus       cId
                       :<|> tableNgramsPut
                       :<|> scoresRecomputeTableNgrams cId
                       :<|> getTableNgramsVersion      cId
                       :<|> apiNgramsAsync             cId

apiNgramsTableDoc :: DocId -> ServerT TableNgramsApi (GargM Env GargError)
apiNgramsTableDoc dId =  getTableNgramsDoc          dId
                    :<|> tableNgramsPut
                    :<|> scoresRecomputeTableNgrams dId
                    :<|> getTableNgramsVersion      dId
                    :<|> apiNgramsAsync             dId

apiNgramsAsync :: NodeId -> ServerT TableNgramsAsyncApi (GargM Env GargError)
apiNgramsAsync _dId =
  serveJobsAPI TableNgramsJob $ \jHandle i -> withTracer (printDebug "tableNgramsPostChartsAsync") jHandle $
    \jHandle' -> tableNgramsPostChartsAsync i jHandle'

-- Did the given list of ngrams changed since the given version?
-- The returned value is versioned boolean value, meaning that one always retrieve the
-- latest version.
-- If the given version is negative then one simply receive the latest version and True.
-- Using this function is more precise than simply comparing the latest version number
-- with the local version number. Indeed there might be no change to this particular list
-- and still the version number has changed because of other lists.
--
-- Here the added value is to make a compromise between precision, computation, and bandwidth:
-- * currentVersion: good computation, good bandwidth, bad precision.
-- * listNgramsChangedSince: good precision, good bandwidth, bad computation.
-- * tableNgramsPull: good precision, good bandwidth (if you use the received data!), bad computation.
listNgramsChangedSince :: HasNodeStory env err m
                       => ListId -> TableNgrams.NgramsType -> Version -> m (Versioned Bool)
listNgramsChangedSince listId ngramsType version
  | version < 0 =
      Versioned <$> currentVersion listId <*> pure True
  | otherwise   =
      tableNgramsPull listId ngramsType version & mapped . v_data %~ (== mempty)
