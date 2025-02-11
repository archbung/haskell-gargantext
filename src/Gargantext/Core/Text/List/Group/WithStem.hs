{-|
Module      : Gargantext.Core.Text.List.Group.WithStem
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FunctionalDependencies #-}

module Gargantext.Core.Text.List.Group.WithStem
  where

import Control.Lens (makeLenses)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Gargantext.API.Ngrams.Types ( toNgramsPatch, NgramsPatch, NgramsTerm(..) )
import Gargantext.Core (Lang(..), Form, Lem, NLPServerConfig)
import Gargantext.Core.Text.List.Group.Prelude ( Stem )
import Gargantext.Core.Text.List.Social.Patch ( addScorePatch )
import Gargantext.Core.Text.List.Social.Prelude ( FlowCont, FlowListScores )
import Gargantext.Core.Text.Terms.Mono.Stem (stem, StemmingAlgorithm(..))
import Gargantext.Prelude

------------------------------------------------------------------------
addScoreStem :: GroupParams
             -> HashSet NgramsTerm
             -> FlowCont NgramsTerm FlowListScores
             -> FlowCont NgramsTerm FlowListScores
addScoreStem groupParams ngrams fl = foldl' addScorePatch fl
                                   $ stemPatches groupParams ngrams

------------------------------------------------------------------------
-- | Main Types
newtype StopSize = StopSize {unStopSize :: Int}
  deriving (Eq, Show)

-- | TODO: group with 2 terms only can be
-- discussed. Main purpose of this is offering
-- a first grouping option to user and get some
-- enriched data to better learn and improve that algo
-- | Lenses instances at the end of this file
data GroupParams = GroupParams { unGroupParams_lang     :: !Lang
                               , unGroupParams_len      :: !Int
                               , unGroupParams_limit    :: !Int
                               , unGroupParams_stopSize :: !StopSize
                               }
                 | GroupIdentity
                 | GroupWithPosTag { _gwl_lang       :: !Lang
                                   , _gwl_nlp_config :: !NLPServerConfig
                                   , _gwl_map        :: !(HashMap Form Lem)
                                   }
  deriving (Eq, Show)

------------------------------------------------------------------------
groupWith :: GroupParams
            -> NgramsTerm
            -> NgramsTerm
groupWith GroupIdentity  t = identity t
groupWith (GroupParams { unGroupParams_lang = l }) t =
                    NgramsTerm
                  $ Text.intercalate " "
                  $ map (stem l PorterAlgorithm)
                  -- . take n
                  $ List.sort
                  -- \$ Set.toList
                  -- \$ Set.fromList
                  -- . (List.filter (\t -> Text.length t > m))
                  $ Text.splitOn " "
                  $ Text.replace "-" " "
                  $ unNgramsTerm t
-- | This lemmatization group done with CoreNLP algo (or others)
groupWith (GroupWithPosTag { _gwl_map = m }) t =
  case HashMap.lookup (unNgramsTerm t) m of
      Nothing -> clean t
      Just t' -> clean $ NgramsTerm t'
  where
    clean (NgramsTerm t'') = NgramsTerm $ Text.replace "-" " " t''

--------------------------------------------------------------------
stemPatches :: GroupParams
           -> HashSet NgramsTerm
           -> [(NgramsTerm, NgramsPatch)]
stemPatches groupParams = patches
                        . Map.fromListWith (<>)
                        . map (\ng -> ( groupWith groupParams ng
                                      , Set.singleton ng
                                      )
                              )
                        . Set.toList

-- | For now all NgramsTerm which have same stem
-- are grouped together
-- Parent is taken arbitrarly for now (TODO use a score like occ)
patches :: Map Stem (HashSet NgramsTerm)
            -> [(NgramsTerm, NgramsPatch)]
patches = catMaybes . map patch . Map.elems

patch :: HashSet NgramsTerm
           -> Maybe (NgramsTerm, NgramsPatch)
patch s = case Set.size s > 1 of
  False -> Nothing
  True  -> do
    let ngrams = Set.toList s
    parent   <- headMay ngrams
    let children = List.tail ngrams
    pure (parent, toNgramsPatch children)

-- | Instances
makeLenses ''GroupParams
