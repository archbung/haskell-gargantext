{-|
Module      : Gargantext.Core.Text.List.Group
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeFamilies           #-}

module Gargantext.Core.Text.List.Group
  where

import Control.Lens (view)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Gargantext.API.Ngrams.Types (NgramsTerm(..))
import Gargantext.Core.Text.List.Group.Prelude
import Gargantext.Core.Text.List.Group.WithScores ( groupWithScores' )
import Gargantext.Core.Text.List.Social.Prelude ( FlowListScores, FlowCont )
import Gargantext.Prelude
------------------------------------------------------------------------
toGroupedTree :: (Ord a, Monoid a, HasSize a)
              => FlowCont NgramsTerm FlowListScores
              -> HashMap NgramsTerm a
              -> FlowCont NgramsTerm (GroupedTreeScores a)
toGroupedTree flc scores =
  groupWithScores' flc scoring
    where
      scoring t = fromMaybe mempty $ HashMap.lookup t scores


------------------------------------------------------------------------
setScoresWithMap :: (Ord a, Ord b, Monoid b) => HashMap NgramsTerm b
                 -> HashMap NgramsTerm (GroupedTreeScores a)
                 -> HashMap NgramsTerm (GroupedTreeScores b)
setScoresWithMap m = setScoresWith (score m)
  where
    score m' t = fromMaybe mempty (HashMap.lookup t m')

setScoresWith :: (Ord a, Ord b)
              => (NgramsTerm -> b)
              -> HashMap NgramsTerm (GroupedTreeScores a)
              -> HashMap NgramsTerm (GroupedTreeScores b)
{-
-- | This Type level lenses solution does not work
setScoresWith f = Map.mapWithKey (\k v -> over gts'_children (setScoresWith f)
                                       $  set  gts'_score    (f k) v
                                 )
-}
setScoresWith f = HashMap.mapWithKey (\k v -> v { _gts'_score    = f k
                                                , _gts'_children = setScoresWith f $ view gts'_children v
                                                }
                                     )
------------------------------------------------------------------------
