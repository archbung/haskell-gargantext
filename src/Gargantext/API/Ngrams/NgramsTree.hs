{-|
Module      : Gargantext.API.Ngrams.NgramsTree
Description : Tree of Ngrams
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.API.Ngrams.NgramsTree
  where

import Data.Aeson.TH (deriveJSON)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Swagger
import Data.Tree
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types (ListType(..), NodeId)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Prelude
import Test.QuickCheck

type Children = Text
type Root = Text

data NgramsTree = NgramsTree { mt_label :: Text
                             , mt_value :: Double
                             , mt_children :: [NgramsTree]
                             }
    deriving (Generic, Show)

toNgramsTree :: Tree (NgramsTerm,Double) -> NgramsTree
toNgramsTree (Node (NgramsTerm l,v) xs) = NgramsTree l v (map toNgramsTree xs)

deriveJSON (unPrefix "mt_") ''NgramsTree

instance ToSchema NgramsTree where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "mt_")
instance Arbitrary NgramsTree
  where
    arbitrary = NgramsTree <$> arbitrary <*> arbitrary <*> arbitrary

toTree :: ListType
       -> HashMap NgramsTerm (Set NodeId)
       -> HashMap NgramsTerm NgramsRepoElement
       -> [NgramsTree]
toTree lt vs m = map toNgramsTree $ unfoldForest buildNode roots
  where
    buildNode r = maybe ((r, value r),[])
                        (\x -> ((r, value r), mSetToList $ _nre_children x))
                        (HashMap.lookup r m)

    value l = maybe 0 (fromIntegral . Set.size) $ HashMap.lookup l vs

    rootsCandidates :: [NgramsTerm]
    rootsCandidates = catMaybes
                    $ List.nub
                    $ map (\(c, c') -> case _nre_root c' of
                                       Nothing -> Just c
                                       _ -> _nre_root c'
                          ) (HashMap.toList m)

    roots = map fst
          $ filter (\(_,l) -> l == lt)
          $ catMaybes
          $ map (\c -> (,) <$> Just c <*> (_nre_list <$> HashMap.lookup c m))
          $ rootsCandidates
