{-|
Module      : Gargantext.Core.Text.Metrics.Utils
Description : Some functions to count.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Core.Text.Metrics.Utils where

import Data.List qualified as L
import Data.Map.Strict qualified as DM
import Gargantext.Prelude

countElem :: (Ord k) => DM.Map k Int -> k -> DM.Map k Int
countElem m e = DM.insertWith (+) e 1 m

freq :: (Ord k) => [k] -> DM.Map k Int
freq = foldl countElem DM.empty

getMaxFromMap :: Ord a => Map a1 a -> [a1]
getMaxFromMap m = go [] Nothing (DM.toList m)
  where
    go ks _        []           = ks
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v < u     = go ks     (Just u) rest
        | v > u     = go [k]    (Just v) rest
        | otherwise = go (k:ks) (Just v) rest


average :: [Double] -> Double
average x = L.sum x / L.genericLength x

average' :: [Int] -> Double
average' x = (L.sum y) / (L.genericLength y) where
    y = L.map fromIntegral x
