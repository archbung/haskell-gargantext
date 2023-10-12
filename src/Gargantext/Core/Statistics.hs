{-|
Module      : Gargantext.Core.Statistics
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Core.Statistics
  where


import Data.Array.IArray (Array, listArray, elems)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Vector.Storable qualified as Vec
import Gargantext.Prelude
import Numeric.Statistics.PCA (pcaReduceN)


data Dimension = Dimension Int

pcaReduceTo :: Ord t
         => Dimension
         -> Map t (Vec.Vector Double)
         -> Map t (Vec.Vector Double)
pcaReduceTo (Dimension d) m = Map.fromList
                             $ zip txts
                             $ elems
                             $ pcaReduceN m'' d
  where
    m'' :: Array Int (Vec.Vector Double)
    m'' = listArray (1, List.length m') m'

    (txts,m') = List.unzip $ Map.toList m
