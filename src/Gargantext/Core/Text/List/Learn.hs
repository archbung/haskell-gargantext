{-|
Module      : Gargantext.Core.Text.List.Learn
Description : Learn to make lists
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

CSV parser for Gargantext corpus files.

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Gargantext.Core.Text.List.Learn
  where

import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.SVM qualified as SVM
import Data.Vector qualified as Vec
import Gargantext.Core
import Gargantext.Core.Text.Metrics.Count (occurrencesWith)
import Gargantext.Core.Types.Main (ListType(..))
import Gargantext.Database.GargDB
import Gargantext.Prelude

------------------------------------------------------------------------
train :: Double -> Double -> SVM.Problem -> IO SVM.Model
train x y = (SVM.train (SVM.CSvc x) (SVM.RBF y))

predict :: SVM.Model -> [Vec.Vector Double] -> IO [Double]
predict m vs = mapM (predict' m) vs
  where
    predict' m' vs' = SVM.predict m' (IntMap.fromList $ (zip [1..]) $ Vec.toList vs')

------------------------------------------------------------------------
trainList :: Double -> Double -> Map ListType [Vec.Vector Double] -> IO SVM.Model
trainList x y = (train x y) . trainList'
  where
    trainList' :: Map ListType [Vec.Vector Double] -> SVM.Problem
    trainList' = mapVec2problem . (Map.mapKeys (fromIntegral . toDBid))

    mapVec2problem :: Map Double [Vec.Vector Double] -> SVM.Problem
    mapVec2problem = List.concat . (map (\(a,as) -> zip (repeat a) as)) . Map.toList . (Map.map vecs2maps)

    vecs2maps :: [Vec.Vector Double] -> [IntMap.IntMap Double]
    vecs2maps = map (IntMap.fromList . (zip [1..]) . Vec.toList)


predictList :: HasCallStack => Model -> [Vec.Vector Double] -> IO [Maybe ListType]
predictList (ModelSVM m _ _) vs = map (Just . fromDBid . round) <$> predict m vs

------------------------------------------------------------------------
data Model = ModelSVM { modelSVM :: SVM.Model
                      , param1 :: Maybe Double
                      , param2 :: Maybe Double
                      }
--{-
instance SaveFile Model
  where
    saveFile' fp (ModelSVM m _ _) = SVM.saveModel m fp

instance ReadFile Model
  where
    readFile' fp = do
      m <- SVM.loadModel fp
      pure $ ModelSVM m Nothing Nothing
--}
------------------------------------------------------------------------
-- | TODO
-- shuffle list
-- split list : train / test
-- grid parameters on best result on test

type Train = Map ListType [Vec.Vector Double]
type Tests = Map ListType [Vec.Vector Double]
type Score = Double
type Param = Double

grid :: (MonadBase IO m)
     => Param -> Param -> Train -> [Tests] -> m (Maybe Model)
grid _ _ _ []  = panicTrace "Gargantext.Core.Text.List.Learn.grid : empty test data"
grid s e tr te = do
  let
    grid' :: (MonadBase IO m)
          => Double -> Double
          -> Train
          -> [Tests]
          -> m (Score, Model)
    grid' x y tr' te' = do
      model'' <- liftBase $ trainList x y tr'

      let
        model' = ModelSVM model'' (Just x) (Just y)

        score' :: [(ListType, Maybe ListType)] -> Map (Maybe Bool) Int
        score' = occurrencesWith (\(a,b) -> (==) <$> Just a <*> b)

        score'' :: Map (Maybe Bool) Int -> Double
        score'' m'' = maybe 0 (\t -> (fromIntegral t)/total) (Map.lookup (Just True) m'')
          where
            total = fromIntegral $ foldl (+) 0 $ Map.elems m''

        getScore m t = do
          let (res, toGuess) = List.unzip
                             $ List.concat
                             $ map (\(k,vs) -> zip (repeat k) vs)
                             $ Map.toList t

          res' <- liftBase $ predictList m toGuess
          pure $ score'' $ score' $ List.zip res res'

      score <- mapM (getScore model') te'
      pure (mean score, model')

  r <- head . List.reverse
            . (List.sortOn fst)
           <$> mapM (\(x,y) -> grid' x y tr te)
                         [(x,y) | x <- [s..e], y <- [s..e]]

  -- printDebug "GRID SEARCH" (map fst r)
  -- printDebug "file" fp
  --fp <- saveFile (ModelSVM model')
  --save best result
  pure $ snd <$> r
