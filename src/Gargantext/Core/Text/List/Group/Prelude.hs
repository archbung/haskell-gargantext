{-|
Module      : Gargantext.Core.Text.List.Group.Prelude
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}

module Gargantext.Core.Text.List.Group.Prelude
  where

import Control.Lens (makeLenses, view, set, over)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Monoid
import Data.Semigroup
import Data.Set qualified as Set
import Gargantext.API.Ngrams.Types (NgramsElement, mkNgramsElement, NgramsTerm(..), RootParent(..), mSetFromList)
import Gargantext.Core.Types (ListType(..))
import Gargantext.Prelude
import Prelude (foldl1)

type Stem = NgramsTerm
------------------------------------------------------------------------
-- | Main Types to group With Scores but preserving Tree dependencies
-- Therefore there is a need of Tree of GroupedTextScores
-- to target continuation type for the flow (FlowCont Text GroupedTreeScores)
data GroupedTreeScores score =
  GroupedTreeScores { _gts'_listType :: !(Maybe ListType)
                    , _gts'_children :: !(HashMap NgramsTerm (GroupedTreeScores score))
                    , _gts'_score    :: !score
                    } deriving (Show, Ord, Eq)

instance (Semigroup a) => Semigroup (GroupedTreeScores a) where
  (<>) (GroupedTreeScores  l1 s1 c1)
       (GroupedTreeScores  l2 s2 c2)
      = GroupedTreeScores (l1 <> l2)
                          (s1 <> s2)
                          (c1 <> c2)

instance (Ord score, Monoid score)
  => Monoid (GroupedTreeScores score) where
    mempty = GroupedTreeScores mempty mempty mempty

makeLenses 'GroupedTreeScores

------------------------------------------------------------------------
-- | Main Classes
class ViewListType a where
  viewListType :: a -> Maybe ListType

class SetListType a where
  setListType :: Maybe ListType -> a -> a

------
class Ord b => ViewScore a b | a -> b where
  viewScore :: a -> b

class ViewScores a b | a -> b where
  viewScores :: a -> b

--------
class ToNgramsElement a where
  toNgramsElement :: a -> [NgramsElement]

class HasTerms a where
  hasTerms :: a -> Set NgramsTerm

------------------------------------------------------------------------
-- | Instances declartion for (GroupedTreeScores a)
instance ViewListType (GroupedTreeScores a) where
  viewListType = view gts'_listType

instance SetListType (GroupedTreeScores a) where
  setListType lt g = over gts'_children (setListType lt)
                $ set gts'_listType lt g

instance SetListType (HashMap NgramsTerm (GroupedTreeScores a)) where
  setListType lt = HashMap.map (set gts'_listType lt)

                            ------
class HasSize a where
  hasSize :: a -> Integer

instance HasSize Double where
  hasSize = round

instance HasSize (Set a) where
  hasSize = fromIntegral . Set.size

instance (HasSize a, Semigroup a) => ViewScore (GroupedTreeScores a) Integer where
  viewScore = hasSize . viewScores

instance Semigroup a=> ViewScores (GroupedTreeScores a) a where
  viewScores g = foldl1 (<>) $ parent : children
    where
      parent   = view gts'_score g
      children = map viewScores $ HashMap.elems $ view gts'_children g
                            ------
instance HasTerms (HashMap NgramsTerm (GroupedTreeScores a)) where
  hasTerms = Set.unions . (map hasTerms) . HashMap.toList

instance HasTerms (NgramsTerm, GroupedTreeScores a) where
  hasTerms (t, g) = Set.singleton t  <> children
    where
      children = Set.unions
               $ map hasTerms
               $ HashMap.toList
               $ view gts'_children g

                            ------
instance ToNgramsElement (HashMap NgramsTerm (GroupedTreeScores a)) where
  toNgramsElement = List.concat . (map toNgramsElement) . HashMap.toList

instance ToNgramsElement (NgramsTerm, GroupedTreeScores a) where
  toNgramsElement (t, gts) = parent : children
    where
      parent = mkNgramsElement t
                               (fromMaybe CandidateTerm $ viewListType gts)
                               Nothing
                               (mSetFromList $ HashMap.keys
                                             $ view gts'_children gts
                               )
      children = List.concat
               $ map (childrenWith t t)
               $ HashMap.toList
               $ view gts'_children gts

      childrenWith root parent' (t', gts') = parent'' : children'
        where
          parent''   = mkNgramsElement t'
                                      (fromMaybe CandidateTerm $ viewListType gts')
                                      (Just $ RootParent root parent')
                                      (mSetFromList $ HashMap.keys
                                                    $ view gts'_children gts'
                                      )
          children' = List.concat
                    $ map (childrenWith root t' )
                    $ HashMap.toList
                    $ view gts'_children gts'
