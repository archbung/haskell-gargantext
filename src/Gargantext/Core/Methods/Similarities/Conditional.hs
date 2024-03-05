{-|
Module      : Gargantext.Core.Methods.Similarities
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Motivation and definition of the @Conditional@ distance.
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE Strict            #-}

module Gargantext.Core.Methods.Similarities.Conditional
  where

import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as Set
import Gargantext.Core.Viz.Graph.Utils (getMax)
import Gargantext.Prelude
import Data.Map.Strict qualified as M
import Gargantext.Core.Methods.Similarities.Accelerate.Conditional (measureConditional)
import Gargantext.Core.Viz.Graph.Index (score, MatrixShape(..))

type HashMap = Map.HashMap
------------------------------------------------------------------------
-- First version as first implementation
-- - qualitatively verified
-- - parallized as main optimization
conditional :: (Ord a, Hashable a, NFData a)
            => HashMap (a,a) Int
            -> HashMap (a,a) Double
conditional m' = Map.fromList $ ((catMaybes results') `using` parList rdeepseq)
  where
    results' = [ let
                  ij = (/) <$> Map.lookup (i,j) m <*> Map.lookup (j,j) m
                    -- proba of i|j, high values means i is more generic than j

                  ji = (/) <$> Map.lookup (i,j) m <*> Map.lookup (i,i) m
                    -- proba of j|i, high values means j is more generic than i

                  in getMax (i,j) ij ji

               | i <- keys
               , j <- keys
               , i < j
               ]
    -- Converting from Int to Double
    m       = Map.map fromIntegral m'

    -- Get the matrix coordinates, removing duplicates
    keys    = Set.toList $ Set.fromList (x <> y)
    (x,y)   = unzip $ Map.keys m

{-
Only for TESTs
-}

conditional_test :: Bool
conditional_test = conditional_test1 == conditional_test2

conditional_test1 :: HashMap (Text,Text) Double
conditional_test1 = conditional $ Map.fromList example_matrix

conditional_test2 :: HashMap (Text,Text) Double
conditional_test2 = Map.fromList
                  $ M.toList
                  $ M.filter (>0)
                  $ score Square measureConditional
                  $ M.fromList example_matrix


example_matrix :: [((Text,Text), Int)]
example_matrix = concat [
          compte "polygon" "polygon"   19

        , compte "polygon" "square"     6
        , compte "polygon" "triangle"  10
        , compte "polygon" "losange"    3

        , compte "triangle" "triangle" 11
        , compte "square"   "square"    7
        , compte "losange" "losange"   15

        , compte "shape" "shape"       10
        , compte "circle" "circle"      6
        , compte "shape" "circle"       3
        , compte "shape" "square"       2
        , compte "polygon" "shape"     10
         
        ]
  where
    compte a b c = if a /= b 
                      then [((a,b),c), ((b,a), c)]
                      else [((a,b),c)]

