{-|
Module      : Gargantext.API.Ngrams.Tools
Description : Tools to manage Ngrams Elements (from the API)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

module Gargantext.API.Ngrams.Tools
  where

-- import Gargantext.Core.NodeStoryFile qualified as NSF
import Control.Lens (_Just, (^.), at, ix, view, At, Index, IxValue)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
-- import GHC.Conc (TVar, readTVar)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.NodeStory
import Gargantext.Core.Types.Main ( ListType(..) )
import Gargantext.Database.Admin.Types.Node ( NodeId, ListId )
import Gargantext.Database.Schema.Ngrams (NgramsType)
import Gargantext.Prelude


mergeNgramsElement :: NgramsRepoElement -> NgramsRepoElement -> NgramsRepoElement
mergeNgramsElement _neOld neNew = neNew

type RootTerm = NgramsTerm


getRepo :: HasNodeStory env err m
         => [ListId] -> m NodeListStory
getRepo listIds = do
  f <- getNodeListStoryMulti
  liftBase $ f listIds
  -- v  <- liftBase $ f listIds
  -- v' <- liftBase $ atomically $ readTVar v
  -- pure $ v'


repoSize :: Ord k1 => NodeStory (Map.Map k1 (Map.Map k2 a)) p
                   -> NodeId
                   -> Map.Map k1 Int
repoSize repo node_id = Map.map Map.size state'
  where
    state' = repo ^. unNodeStory
                   . at node_id . _Just
                   . a_state


getNodeStory :: HasNodeStory env err m
           => ListId -> m ArchiveList
getNodeStory l = do
  f <- getNodeListStory
  liftBase $ f l
  -- v  <- liftBase $ f l
  -- pure v


getNodeListStory :: HasNodeStory env err m
                 => m (NodeId -> IO ArchiveList)
getNodeListStory = do
  env <- view hasNodeStory
  pure $ view nse_getter env


getNodeListStoryMulti :: HasNodeStory env err m
                      => m ([NodeId] -> IO NodeListStory)
getNodeListStoryMulti = do
  env <- view hasNodeStory
  pure $ view nse_getter_multi env



listNgramsFromRepo :: [ListId]
                   -> NgramsType
                   -> NodeListStory
                   -> HashMap NgramsTerm NgramsRepoElement
listNgramsFromRepo nodeIds ngramsType repo =
  HM.fromList $ Map.toList
              $ Map.unionsWith mergeNgramsElement ngrams
    where
      ngrams = [ repo
               ^. unNodeStory
                . at nodeId . _Just
                . a_state
                . ix ngramsType
                | nodeId <- nodeIds
                ]

-- TODO-ACCESS: We want to do the security check before entering here.
--              Add a static capability parameter would be nice.
--              Ideally this is the access to `repoVar` which needs to
--              be properly guarded.
getListNgrams :: HasNodeStory env err m
              => [ListId] -> NgramsType
              -> m (HashMap NgramsTerm NgramsRepoElement)
getListNgrams nodeIds ngramsType = listNgramsFromRepo nodeIds ngramsType
                                 <$> getRepo nodeIds


getTermsWith :: (HasNodeStory env err m, Eq a, Hashable a)
          => (NgramsTerm -> a) -> [ListId]
          -> NgramsType -> Set ListType
          -> m (HashMap a [a])
getTermsWith f ls ngt lts  = HM.fromListWith (<>)
                      <$> map toTreeWith
                      <$> HM.toList
                      <$> HM.filter (\f' -> Set.member (fst f') lts)
                      <$> mapTermListRoot ls ngt
                      <$> getRepo ls
  where
    toTreeWith (t, (_lt, maybeRoot)) = case maybeRoot of
      Nothing -> (f t, [])
      Just  r -> (f r, [f t])



mapTermListRoot :: [ListId]
                -> NgramsType
                -> NodeListStory
                -> HashMap NgramsTerm (ListType, Maybe NgramsTerm)
mapTermListRoot nodeIds ngramsType repo =
      (\nre -> (_nre_list nre, _nre_root nre))
  <$> listNgramsFromRepo nodeIds ngramsType repo




filterListWithRootHashMap :: ListType
                          -> HashMap NgramsTerm (ListType, Maybe NgramsTerm)
                          -> HashMap NgramsTerm (Maybe RootTerm)
filterListWithRootHashMap lt m = snd <$> HM.filter isMapTerm m
  where
    isMapTerm (l, maybeRoot) = case maybeRoot of
      Nothing -> l == lt
      Just  r -> case HM.lookup r m of
        Nothing -> panicTrace $ "[Garg.API.Ngrams.Tools] filterListWithRootHashMap, unknown key: " <> unNgramsTerm r
        Just  (l',_) -> l' == lt

filterListWithRoot :: [ListType]
                   -> HashMap NgramsTerm (ListType, Maybe NgramsTerm)
                   -> HashMap NgramsTerm (Maybe RootTerm)
filterListWithRoot lt m = snd <$> HM.filter isMapTerm m
  where
    isMapTerm (l, maybeRoot) = case maybeRoot of
      Nothing -> l `elem` lt
      Just  r -> case HM.lookup r m of
        Nothing -> panicTrace $ "[Garg.API.Ngrams.Tools] filterListWithRoot, unknown key: " <> unNgramsTerm r
        Just  (l',_) -> elem l' lt

groupNodesByNgrams :: ( Ord a
                      , At root_map
                      , Index root_map ~ NgramsTerm
                      , IxValue root_map ~ Maybe RootTerm
                      )
                   => root_map
                   -> HashMap NgramsTerm (Set a)
                   -> HashMap NgramsTerm (Set a)
groupNodesByNgrams syn occs = HM.fromListWith (<>) occs'
  where
    occs' = map toSyn (HM.toList occs)
    toSyn (t,ns) = case syn ^. at t of
      Nothing -> panicTrace $ "[Garg.API.Ngrams.Tools.groupNodesByNgrams] unknown key: " <> unNgramsTerm t
      Just  r -> case r of
        Nothing  -> (t, ns)
        Just  r' -> (r',ns)

newtype Diagonal = Diagonal Bool

getCoocByNgrams :: Diagonal
                -> HashMap NgramsTerm (Set NodeId)
                -> HashMap (NgramsTerm, NgramsTerm) Int
getCoocByNgrams = getCoocByNgrams' identity


getCoocByNgrams' :: (Hashable a, Ord a, Ord c)
                 => (b -> Set c)
                 -> Diagonal
                 -> HashMap a b
                 -> HashMap (a, a) Int
getCoocByNgrams' f (Diagonal diag) m =
  HM.fromList [( (t1,t2)
               , maybe 0 Set.size $ Set.intersection
                                 <$> (fmap f $ HM.lookup t1 m)
                                 <*> (fmap f $ HM.lookup t2 m)
               )
              | (t1,t2) <- if diag
                              then [ (x,y) | x <- ks, y <- ks, x <= y]
                                   -- TODO if we keep a Data.Map here it might be
                                   -- more efficient to enumerate all the y <= x.
                              else
                                listToCombi identity ks
              ]

  where
    ks = HM.keys m

-- TODO k could be either k1 or k2 here
getCoocByNgrams'' :: (Hashable k, Ord k, Ord contexts)
                  => Diagonal
                  -> (contextA -> Set contexts, contextB -> Set contexts)
                  -> (HashMap k contextA, HashMap k contextB)
                  -> HashMap (k, k) Int
getCoocByNgrams'' (Diagonal diag) (f1,f2) (m1,m2) =
  HM.fromList [( (t1,t2)
               , maybe 0 Set.size $ Set.intersection
                                 <$> (fmap f1 $ HM.lookup t1 m1)
                                 <*> (fmap f2 $ HM.lookup t2 m2)
               )
              | (t1,t2) <- if diag
                              then
                                [ (x,y) | x <- ks1, y <- ks2, x <= y]
                                   -- TODO if we keep a Data.Map here it might be
                                   -- more efficient to enumerate all the y <= x.
                              else
                                [ (x,y) | x <- ks1, y <- ks2, x < y]
                                -- TODO check optim
                                -- listToCombi identity ks1
              ]
  where
    ks1 = HM.keys m1
    ks2 = HM.keys m2



------------------------------------------


-- migrateFromDirToDb :: (HasNodeStory env err m) -- , HasNodeStory env err m)
--                    => m ()
-- migrateFromDirToDb = do
--   pool <- view connPool
--   withResource pool $ \c -> do
--     listIds <- liftBase $ getNodesIdWithType c NodeList
--     -- printDebug "[migrateFromDirToDb] listIds" listIds
--     (NodeStory nls) <- NSF.getRepoReadConfig listIds
--     -- printDebug "[migrateFromDirToDb] nls" nls
--     _ <- mapM (\(nId, a) -> do
--                   n <- liftBase $ nodeExists c nId
--                   case n of
--                     False -> pure ()
--                     True  -> liftBase $ upsertNodeStories c nId a
--               ) $ Map.toList nls
--     --_ <- nodeStoryIncs (Just $ NodeStory nls) listIds
--     pure ()
