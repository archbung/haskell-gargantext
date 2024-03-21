{-|
Module      : Gargantext.API.Ngrams.Prelude
Description : Tools to manage Ngrams Elements (from the API)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TypeFamilies #-}

module Gargantext.API.Ngrams.Prelude
  where

import Control.Lens (view)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Map.Strict (fromList)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Gargantext.API.Ngrams (getNgramsTableMap)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.NodeStory.Types ( HasNodeStory )
import Gargantext.Core.Text.Context (TermList)
import Gargantext.Core.Text.List.Social.Prelude ( unPatchMapToHashMap )
import Gargantext.Core.Text.Ngrams (NgramsType, ngramsTypes)
import Gargantext.Core.Types.Main ( ListType )
import Gargantext.Database.Admin.Types.Node (ListId)
import Gargantext.Prelude


------------------------------------------------------------------------
getNgramsList :: HasNodeStory env err m
              => ListId -> m NgramsList
getNgramsList lId = fromList
       <$> zip ngramsTypes
       <$> mapM (getNgramsTableMap lId) ngramsTypes

getTermList :: HasNodeStory env err m
            => ListId -> ListType -> NgramsType -> m (Maybe TermList)
getTermList lId listType ngramsType = do
  ngramsList <- getNgramsList lId
  pure $ toTermList listType ngramsType ngramsList


------------------------------------------------------------------------
-- | Tools
-- Usage example: toTermList MapTerm NgramsTerms ngramsList
toTermList :: ListType -> NgramsType -> NgramsList -> Maybe TermList
toTermList lt nt nl = toTermList' lt <$> Map.lookup nt nl
  where
    toTermList' :: ListType -> Versioned NgramsTableMap -> TermList
    toTermList' lt' = (toTermList'' lt') . Map.toList . view v_data

    toTermList'' :: ListType -> [(NgramsTerm, NgramsRepoElement)] -> TermList
    toTermList'' lt'' ns = Map.toList
                       $ Map.mapKeys toTerm
                       $ Map.fromListWith (<>) (roots' <> children')
      where
        toTerm = Text.splitOn " " . unNgramsTerm

        (roots, children) = List.partition (\(_t, nre) -> isNothing (view nre_root nre))
                          $ List.filter (\(_t,nre) -> view nre_list nre == lt'') ns

        roots'    = map (\(t,nre) -> (t, map toTerm $ unMSet $ view nre_children nre )) roots

        children' = catMaybes
                  $ map (\(t,nre) -> (,) <$> view nre_root nre
                                         <*> Just (map toTerm $ [t]
                                                             <> (unMSet $ view nre_children nre)
                                                  )
                        ) children

------------------------------------------
patchMSet_toList :: (Ord a, Hashable a) => PatchMSet a -> [(a,AddRem)]
patchMSet_toList = HM.toList . unPatchMapToHashMap . unPatchMSet

unMSet :: MSet a -> [a]
unMSet (MSet a) = Map.keys a
