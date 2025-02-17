{-|
Module      : Gargantext.Core.Viz.Phylo.SynchronicClustering
Description : Module dedicated to the adaptative synchronic clustering of a Phylo.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Gargantext.Core.Viz.Phylo.SynchronicClustering where

import Control.Lens hiding (Level)
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.List (intersect, nub)
import Data.Map  (fromList, fromListWith, foldlWithKey, (!), insert, empty, restrictKeys, elems, mapWithKey, member, unionWith)
import Data.Map qualified as Map
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.PhyloExport (processDynamics)
import Gargantext.Core.Viz.Phylo.PhyloTools
import Gargantext.Core.Viz.Phylo.TemporalMatching (weightedLogJaccard', filterDiago, reduceDiagos)
import Gargantext.Prelude hiding (empty)


-------------------------
-- | New Level Maker | --
-------------------------

mergeGroups :: [Cooc] -> PhyloGroupId -> Map PhyloGroupId PhyloGroupId -> [PhyloGroup] -> PhyloGroup
mergeGroups coocs id mapIds childs = 
    let ngrams = (sort . nub . concat) $ map _phylo_groupNgrams childs
        counts = foldl (\acc count -> unionWith (+) acc count) empty $ map _phylo_groupRootsCount childs
    in PhyloGroup (fst $ fst id) (_phylo_groupPeriod' $ head' "mergeGroups" childs)
                  (snd $ fst id) (snd id) ""
                  (sum $ map _phylo_groupSupport childs) 
                  (fmap sum $ sequence 
                            $ map _phylo_groupWeight childs)
                  (concat $ map _phylo_groupSources childs) 
                  ngrams
                  (ngramsToCooc ngrams coocs) 
                  (ngramsToDensity ngrams coocs counts)
                  -- todo add density here
                  ((snd $ fst id),bId)
                  (mergeMeta bId childs)
                  counts 
                  [] (map (\g -> (getGroupId g, 1)) childs)
                  (updatePointers $ concat $ map _phylo_groupPeriodParents childs)
                  (updatePointers $ concat $ map _phylo_groupPeriodChilds  childs)
                  (mergeAncestors $ concat $ map _phylo_groupAncestors childs)
                  (updatePointers' $ concat $ map _phylo_groupPeriodMemoryParents childs)
                  (updatePointers' $ concat $ map _phylo_groupPeriodMemoryChilds  childs)
    where
        --------------------
        bId :: [Int]
        bId = mergeBranchIds $ map (\g -> snd $ g ^. phylo_groupBranchId) childs
        --------------------
        updatePointers :: [Pointer] -> [Pointer]
        updatePointers pointers = map (\(pId,w) -> (mapIds ! pId,w)) pointers
        updatePointers' :: [Pointer'] -> [Pointer']
        updatePointers' pointers = map (\(pId,(t,w)) -> (mapIds ! pId,(t,w))) pointers
        --------------------
        mergeAncestors :: [Pointer] -> [Pointer]
        mergeAncestors pointers = Map.toList $ fromListWith max pointers

addPhyloScale :: Scale -> Phylo -> Phylo
addPhyloScale lvl phylo = 
  over ( phylo_periods .  traverse ) 
       (\phyloPrd -> phyloPrd & phylo_periodScales 
                        %~ (insert (phyloPrd ^. phylo_periodPeriod, lvl) 
                                   (PhyloScale (phyloPrd ^. phylo_periodPeriod) (phyloPrd ^. phylo_periodPeriodStr) lvl empty))) phylo


toNextScale :: Phylo -> [PhyloGroup] -> Phylo
toNextScale phylo groups =
    let curLvl = getLastLevel phylo
        oldGroups = fromList $ map (\g -> (getGroupId g, getLevelParentId g)) groups
        newGroups = concat $ groupsToBranches'
                  $ fromList $ map (\g -> (getGroupId g, g))
                  $ foldlWithKey (\acc id groups' ->
                        --  4) create the parent group
                        let parent = mergeGroups (elems $ restrictKeys (getCoocByDate phylo) $ periodsToYears [(fst . fst) id]) id oldGroups groups'
                        in  acc ++ [parent]) []
                  --  3) group the current groups by parentId
                  $ fromListWith (++) $ map (\g -> (getLevelParentId g, [g])) groups

        newPeriods = fromListWith (++) $ map (\g -> (g ^. phylo_groupPeriod, [g])) newGroups
    in  traceSynchronyEnd 
      $ over ( phylo_periods . traverse . phylo_periodScales . traverse
             --  6) update each period at curLvl + 1
             . filtered (\phyloLvl -> phyloLvl ^. phylo_scaleScale == (curLvl + 1)))
             --  7) by adding the parents
             (\phyloLvl -> 
                if member (phyloLvl ^. phylo_scalePeriod) newPeriods
                    then phyloLvl & phylo_scaleGroups
                            .~ fromList (map (\g -> (getGroupId g, g)) $ newPeriods ! (phyloLvl ^. phylo_scalePeriod))
                    else phyloLvl)
      --  2) add the curLvl + 1 PhyloScale to the phylo
      $ addPhyloScale (curLvl + 1)
      --  1) update the current groups (with level parent pointers) in the phylo
      $ updatePhyloGroups curLvl (fromList $ map (\g -> (getGroupId g, g)) groups) phylo 

--------------------
-- | Clustering | --
--------------------

toPairs :: SynchronyStrategy -> [PhyloGroup] -> [(PhyloGroup,PhyloGroup)]
toPairs strategy groups = case strategy of 
  MergeRegularGroups -> pairs
                      $ filter (\g -> all (== 3) $ (g ^. phylo_groupMeta) ! "dynamics") groups
  MergeAllGroups -> pairs groups
  where 
    pairs :: [PhyloGroup] -> [(PhyloGroup,PhyloGroup)]
    pairs gs = filter (\(g,g') -> (not . null) $ intersect (g ^. phylo_groupNgrams) (g' ^. phylo_groupNgrams)) (listToCombi' gs)


toDiamonds :: [PhyloGroup] -> [[PhyloGroup]]
toDiamonds groups = foldl' (\acc groups' ->
                        acc ++ ( elems
                               $ Map.filter (\v -> length v > 1)
                               $ fromListWith (++)
                               $ foldl' (\acc' g -> 
                                    acc' ++ (map (\(id,_) -> (id,[g]) ) $ g ^. phylo_groupPeriodChilds)) [] groups')) []
                  $ elems
                  $ Map.filter (\v -> length v > 1)
                  $ fromListWith (++)
                  $ foldl' (\acc g -> acc ++ (map (\(id,_) -> (id,[g]) ) $ g ^. phylo_groupPeriodParents)  ) [] groups


groupsToEdges :: PhyloSimilarity -> Synchrony -> Double -> Map Int Double -> [PhyloGroup] -> [((PhyloGroup,PhyloGroup),Double)]
groupsToEdges prox sync nbDocs diago groups =
    case sync of
        ByProximityThreshold  thr sens _ strat ->
            filter (\(_,w) -> w >= thr)
          $ toEdges sens
          $ toPairs strat groups
        ByProximityDistribution sens strat -> 
            let diamonds = sortOn snd 
                         $ toEdges sens $ concat
                         $ map (\gs -> toPairs strat gs) $ toDiamonds groups 
             in take (div (length diamonds) 2) diamonds
    where 
        toEdges :: Double -> [(PhyloGroup,PhyloGroup)] -> [((PhyloGroup,PhyloGroup),Double)]
        toEdges sens edges = 
            case prox of
                WeightedLogJaccard _ _ -> map (\(g,g') -> 
                                                     ((g,g'), weightedLogJaccard' (sens) nbDocs diago
                                                                  (g ^. phylo_groupNgrams) (g' ^. phylo_groupNgrams))) edges
                WeightedLogSim _ _     -> map (\(g,g') -> 
                                                     ((g,g'), weightedLogJaccard' (1 / sens) nbDocs diago
                                                                  (g ^. phylo_groupNgrams) (g' ^. phylo_groupNgrams))) edges

                _ -> undefined

toParentId :: PhyloGroup -> PhyloGroupId
toParentId child = ((child ^. phylo_groupPeriod, child ^. phylo_groupScale + 1), child ^. phylo_groupIndex) 


reduceGroups :: PhyloSimilarity -> Synchrony -> Map Date Double -> Map Date Cooc -> [PhyloGroup] -> [PhyloGroup]
reduceGroups prox sync docs diagos branch =
    --  1) reduce a branch as a set of periods & groups
    let periods = fromListWith (++)
                 $ map (\g -> (g ^. phylo_groupPeriod,[g])) branch
    in  (concat . concat . elems)
      -- TODO : ajouter un parallelisme
      $ mapWithKey (\prd groups -> 
            --  2) for each period, transform the groups as a proximity graph filtered by a threshold
            let diago = reduceDiagos $ filterDiago diagos [prd]
                edgesLeft  = fromList $ groupsToEdges prox sync ((sum . elems) $ restrictKeys docs $ periodsToYears [prd]) diago groups
                edgesRight = fromList $  map (\((k1,k2),v) -> ((k2,k1),v)) 
                           $ groupsToEdges prox sync ((sum . elems) $ restrictKeys docs $ periodsToYears [prd]) diago (reverse groups)
                mergedEdges = Map.toList 
                            $ unionWith (\v1 v2 -> if v1 >= v2
                                                        then v1
                                                        else v2) edgesLeft edgesRight
                -- 3) reduce the graph a a set of related components                            
                clusters = toRelatedComponents groups mergedEdges
             in map (\comp -> 
                    --  4) add to each groups their futur level parent group
                    let parentId = toParentId (head' "parentId" comp)
                    in  map (\g -> g & phylo_groupScaleParents %~ (++ [(parentId,1)]) ) comp )
              $ clusters) periods 

chooseClusteringStrategy :: Synchrony -> [[PhyloGroup]] -> [[PhyloGroup]]
chooseClusteringStrategy sync branches = case sync of
  ByProximityThreshold _ _ scope _ -> case scope of 
      SingleBranch -> branches
      SiblingBranches -> groupBy (\g g' -> (last' "chooseClusteringStrategy" $ (g  ^. phylo_groupMeta) ! "breaks") 
                                        == (last' "chooseClusteringStrategy" $ (g' ^. phylo_groupMeta) ! "breaks"))
                       $ sortOn _phylo_groupBranchId $ concat branches
      AllBranches -> [concat branches]
  ByProximityDistribution _ _ -> branches


levelUpAncestors :: [PhyloGroup] -> [PhyloGroup]
levelUpAncestors groups =
  -- 1) create an associative map of (old,new) ids
  let ids' = fromList $ map (\g -> (getGroupId g, fst $ head' "levelUpAncestors" ( g ^. phylo_groupScaleParents))) groups 
   in map (\g -> 
        let id' = ids' ! (getGroupId g)
            ancestors  = g ^. phylo_groupAncestors
            -- 2) level up the ancestors ids and filter the ones that will be merged
            ancestors' = filter (\(id,_) -> id /= id') $ map (\(id,w) -> (ids' ! id,w)) ancestors 
         in g & phylo_groupAncestors .~ ancestors'
      ) groups

synchronicClustering :: Phylo -> Phylo
synchronicClustering phylo =
    let prox = similarity $ getConfig phylo
        sync = phyloSynchrony $ getConfig phylo
        docs = getDocsByDate phylo
        diagos = map coocToDiago $ getCoocByDate phylo
        newBranches  = map (\branch -> reduceGroups prox sync docs diagos branch) 
                     $ map processDynamics
                     $ chooseClusteringStrategy sync
                     $ phyloLastScale 
                     $ traceSynchronyStart phylo
        newBranches' = newBranches `using` parList rdeepseq
     in toNextScale phylo $ levelUpAncestors $ concat newBranches'


-- synchronicDistance :: Phylo -> Level -> String
-- synchronicDistance phylo lvl = 
--     foldl' (\acc branch -> 
--              acc <> (foldl' (\acc' period ->
--                               acc' <> let prox  = phyloProximity $ getConfig phylo
--                                           sync  = phyloSynchrony $ getConfig phylo
--                                           docs  = _phylo_timeDocs phylo
--                                           prd   = _phylo_groupPeriod $ head' "distance" period
--                                           edges = groupsToEdges prox 0.1 (_bpt_sensibility sync) 
--                                                   ((sum . elems) $ restrictKeys docs $ periodsToYears [_phylo_groupPeriod $ head' "distance" period]) period
--                                       in foldl' (\mem (_,w) -> 
--                                           mem <> show (prd)
--                                               <> "\t"
--                                               <> show (w)
--                                               <> "\n"
--                                         ) "" edges 
--                      ) ""  $ elems $ groupByField _phylo_groupPeriod branch)
--     ) "period\tdistance\n" $ elems $ groupByField _phylo_groupBranchId $ getGroupsFromLevel lvl phylo
