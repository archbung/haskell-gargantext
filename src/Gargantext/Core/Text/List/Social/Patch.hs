{-|
Module      : Gargantext.Core.Text.List.Social.Patch
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Core.Text.List.Social.Patch
  where

import Control.Lens hiding (cons)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Patch.Class qualified as Patch (Replace(..))
import Gargantext.API.Ngrams.Prelude (unMSet, patchMSet_toList)
import Gargantext.API.Ngrams.Types ( NgramsTerm, nre_children, nre_list, MSet, NgramsPatch(..) )
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Text.Ngrams (NgramsType(..))
import Gargantext.Database.Admin.Types.Node ( ListId )
import Gargantext.Prelude

addScorePatches :: NgramsType -> [ListId]
                -> FlowCont NgramsTerm FlowListScores
                -> Map ListId (Map NgramsType [HashMap NgramsTerm NgramsPatch])
                -> FlowCont NgramsTerm FlowListScores
addScorePatches nt listes fl repo =
  foldl' (addScorePatchesList nt repo) fl listes


addScorePatchesList :: NgramsType
                   -> Map ListId (Map NgramsType [HashMap NgramsTerm NgramsPatch])
                   -> FlowCont NgramsTerm FlowListScores
                   -> ListId
                   -> FlowCont NgramsTerm FlowListScores
addScorePatchesList nt repo fl lid =
  foldl' addScorePatch fl patches
  where
    patches = maybe [] (concatMap HashMap.toList) patches'

    patches' = do
      lists      <- Map.lookup lid repo
      mapPatches <- Map.lookup nt lists
      pure mapPatches


addScorePatch :: FlowCont NgramsTerm FlowListScores
              -> (NgramsTerm , NgramsPatch)
              -> FlowCont NgramsTerm FlowListScores

{- | Case of changing listType only. Patches look like:

This patch move "problem" from MapTerm to CandidateTerm
,fromList [(NgramsTerm {unNgramsTerm = "problem"},NgramsPatch {_patch_children = PatchMSet (PatchMap (fromList [])), _patch_list = Replace {_old = MapTerm, _new = CandidateTerm}})]

This patch move "paper" from MapTerm to StopTerm
,fromList [(NgramsTerm {unNgramsTerm = "paper"},NgramsPatch {_patch_children = PatchMSet (PatchMap (fromList [])), _patch_list = Replace {_old = MapTerm, _new = StopTerm}})]])])]

Children are not modified in this specific case.
-}
-- | Old list get -1 score
-- New list get +1 score
-- Hence others lists lay around 0 score
addScorePatch fl (t, (NgramsPatch { _patch_children
                                  , _patch_list = Patch.Replace old_list new_list })) =
  -- Adding new 'Children' score
  addScorePatch fl' (t, NgramsPatch { _patch_children
                                    , _patch_list = Patch.Keep })
    where
      -- | Adding new 'ListType' score
      fl' = fl & flc_scores . at t %~ (score fls_listType old_list (-1))
          & flc_scores . at t %~ (score fls_listType new_list ( 1))
          & flc_cont   %~ (HashMap.delete t)

-- | Patching existing Ngrams with children
addScorePatch fl (p, NgramsPatch { _patch_children
                                 , _patch_list = Patch.Keep }) =
  foldl' addChild fl $ patchMSet_toList _patch_children
    where
      -- | Adding a child
      addChild fl' (t, Patch.Replace Nothing (Just _)) = doLink ( 1) p t fl'
      -- | Removing a child
      addChild fl' (t, Patch.Replace (Just _) Nothing) = doLink (-1) p t fl'

      -- | This case should not happen: does Nothing
      addChild fl' _ = fl'

-- | Inserting a new Ngrams
addScorePatch fl (t, NgramsReplace { _patch_old = Nothing
                                   , _patch_new = Just nre }) =
  childrenScore 1 t (nre ^. nre_children)
  $ fl & flc_scores . at t %~ (score fls_listType $ nre ^. nre_list) 1
       & flc_cont   %~ (HashMap.delete t)

addScorePatch fl (t, NgramsReplace { _patch_old = Just old_nre
                                   , _patch_new = maybe_new_nre }) =
  let fl' = childrenScore (-1) t (old_nre ^. nre_children)
            $ fl & flc_scores . at t %~ (score fls_listType $ old_nre ^. nre_list) (-1)
                 & flc_cont   %~ (HashMap.delete t)
    in case maybe_new_nre of
      Nothing      -> fl'
      Just new_nre -> addScorePatch fl' (t, NgramsReplace { _patch_old = Nothing
                                                          , _patch_new = Just new_nre })

addScorePatch fl (_, NgramsReplace { _patch_old = Nothing
                                   , _patch_new = Nothing }) = fl

-------------------------------------------------------------------------------
-- | Utils
childrenScore :: Int
              -> NgramsTerm
              -> MSet NgramsTerm
              -> FlowCont NgramsTerm FlowListScores
              -> FlowCont NgramsTerm FlowListScores
childrenScore n parent children' fl =
  foldl' add' fl $ unMSet children'
    where
        add' fl' t = doLink n parent t fl'

------------------------------------------------------------------------
doLink :: (Ord a, Hashable a)
       => Int
       -> NgramsTerm
       -> a
       -> FlowCont a FlowListScores
       -> FlowCont a FlowListScores
doLink n parent child fl' = fl' & flc_scores . at child %~ (score fls_parents parent n)


score :: (Monoid a, At m, Semigroup (IxValue m))
      => ((m -> Identity m) -> a -> Identity b)
      -> Index m -> IxValue m -> Maybe a -> Maybe b
score field list' n m = (Just mempty <> m)
                        & _Just
                        . field
                        . at list'
                        %~ (<> Just n)

------------------------------------------------------------------------
