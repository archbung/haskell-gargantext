{-|
Module      : Gargantext.Core.Text.Metrics.TFICF
Description : TFICF Ngrams tools
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Definition of TFICF : Term Frequency - Inverse of Context Frequency

TFICF is a generalization of [TFIDF](https://en.wikipedia.org/wiki/Tf%E2%80%93idf).

-}


module Gargantext.Core.Text.Metrics.TFICF ( TFICF
                                     , TficfContext(..)
                                     , Total(..)
                                     , Count(..)
                                     , tficf
                                     , sortTficf
                                     )
  where

import Data.List qualified as List
import Data.Map.Strict (toList)
import Data.Ord qualified as DO (Down(..))
import Gargantext.Core.Types (Ordering(..))
import Gargantext.Prelude hiding (Down, Ordering, toList)

path :: Text
path = "[G.T.Metrics.TFICF]"

type TFICF = Double

data TficfContext n m = TficfInfra n m
                      | TficfSupra n m
  deriving (Show)

data Total = Total {unTotal :: !Double}
data Count = Count {unCount :: !Double}

tficf :: TficfContext Count Total
      -> TficfContext Count Total
      -> TFICF
tficf (TficfInfra (Count ic) (Total it) )
      (TficfSupra (Count sc) (Total st) )
            | it >= ic && st >= sc && it <= st = (it/ic) * log (st/sc)
            | otherwise                        = panicTrace
                                               $ "[ERR]"
                                               <> path
                                               <> " Frequency impossible"
tficf _ _ = panicTrace $ "[ERR]" <> path <> "Undefined for these contexts"


sortTficf :: Ordering
          -> Map Text Double
          -> [(Text, Double)]
sortTficf Down = List.sortOn (DO.Down . snd) . toList
sortTficf Up   = List.sortOn snd . toList

