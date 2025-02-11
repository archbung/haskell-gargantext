{-|
Module      : Gargantext.Core.Text.List.WithScores
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE InstanceSigs           #-}

module Gargantext.Core.Text.List.Group.WithScores
  where

import Control.Lens (view, set, over)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Semigroup
import Gargantext.API.Ngrams.Types (NgramsTerm(..))
import Gargantext.Core.Text.List.Group.Prelude
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Prelude

------------------------------------------------------------------------
-- | Main function
groupWithScores' :: (Eq a, Ord a, Monoid a, HasSize a)
                => FlowCont NgramsTerm FlowListScores
                -> (NgramsTerm -> a)
                -> FlowCont NgramsTerm (GroupedTreeScores a)
groupWithScores' flc scores = FlowCont  groups orphans
  where
    -- parent/child relation is inherited from social lists
    groups  = HashMap.filter ((0 <) . viewScore)
            $ toGroupedTree'
            $ toMapMaybeParent scores
            $ (view flc_scores flc <> view flc_cont flc)

    -- orphans should be filtered already then becomes empty
    orphans = mempty
   

------------------------------------------------------------------------
toMapMaybeParent :: (Eq a, Ord a, Monoid a)
                 => (NgramsTerm -> a)
                 -> HashMap NgramsTerm FlowListScores
                 -> HashMap (Maybe Parent) (HashMap NgramsTerm (GroupedTreeScores a))
toMapMaybeParent f =  HashMap.fromListWith (<>)
                   . (map (fromScores'' f))
                   .  HashMap.toList

fromScores'' :: (Eq a, Ord a, Monoid a)
             => (NgramsTerm -> a)
             -> (NgramsTerm, FlowListScores)
             -> (Maybe Parent, HashMap NgramsTerm (GroupedTreeScores a))
fromScores'' f' (t, fs) = ( maybeParent
                          , HashMap.fromList [( t, set gts'_score (f' t)
                                             $ set gts'_listType maybeList mempty
                                         )]
                          )
    where
     maybeParent = keyWithMaxValue $ view fls_parents  fs
     maybeList   = keyWithMaxValue $ view fls_listType fs

------------------------------------------------------------------------
toGroupedTree' :: Eq a
              => HashMap (Maybe Parent) (HashMap NgramsTerm (GroupedTreeScores a))
              -> HashMap Parent (GroupedTreeScores a)
toGroupedTree' m = case HashMap.lookup Nothing m of
  Nothing  -> mempty
  Just  m' -> toGroupedTree'' m m'

filterGroupedTree :: (GroupedTreeScores a -> Bool)
                  -> HashMap Parent (GroupedTreeScores a)
                  -> HashMap Parent (GroupedTreeScores a)
filterGroupedTree f = HashMap.filter f


toGroupedTree'' :: Eq a => HashMap (Maybe Parent) (HashMap NgramsTerm (GroupedTreeScores a))
               -> (HashMap NgramsTerm (GroupedTreeScores a))
               ->  HashMap Parent (GroupedTreeScores a)
toGroupedTree'' m notEmpty
  | notEmpty == mempty = mempty
  | otherwise = HashMap.mapWithKey (addGroup m) notEmpty
    where
      addGroup m' k v = over gts'_children ( (toGroupedTree'' m')
                                           . (HashMap.union ( fromMaybe mempty
                                                        $ HashMap.lookup (Just k) m'
                                                        )
                                             )
                                           )
                                           v

