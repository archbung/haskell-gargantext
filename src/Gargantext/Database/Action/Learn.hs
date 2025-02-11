{-|
Module      : Gargantext.Database.Learn
Description : Learn Small Data Analytics with big data connection (DB)
opyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE MonoLocalBinds    #-}

module Gargantext.Database.Action.Learn
  where

import Data.List qualified as List
import Data.Maybe
import Data.Text qualified as Text
import Gargantext.Core
import Gargantext.Core.Text.Learn
import Gargantext.Core.Types.Query (Offset, Limit(..))
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (DBCmd)
import Gargantext.Database.Query.Facet
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Prelude

data FavOrTrash = IsFav | IsTrash
  deriving (Eq)


moreLike :: (HasDBid NodeType, HasNodeError err)
         => CorpusId   -> Maybe Offset -> Maybe Limit -> Maybe OrderBy
         -> FavOrTrash -> DBCmd err [FacetDoc]
moreLike cId o _l order ft = do
  priors <- getPriors ft cId
  moreLikeWith cId o (Just 3) order ft priors

---------------------------------------------------------------------------
getPriors :: (HasDBid NodeType, HasNodeError err)
          => FavOrTrash -> CorpusId -> DBCmd err (Events Bool)
getPriors ft cId = do

  docs_fav   <- filter (\(FacetDoc _ _ _ _ f _ _) -> f == Just 2)
              <$> runViewDocuments cId False Nothing Nothing Nothing Nothing Nothing

  docs_trash <- List.take (List.length docs_fav)
            <$> runViewDocuments cId True Nothing Nothing Nothing Nothing Nothing


  let priors = priorEventsWith text (fav2bool ft) (  List.zip (repeat False) docs_fav
                                      <> List.zip (repeat True ) docs_trash
                                      )
  pure priors


moreLikeWith :: (HasDBid NodeType, HasNodeError err)
             => CorpusId   -> Maybe Offset -> Maybe Limit -> Maybe OrderBy
             -> FavOrTrash -> Events Bool  -> DBCmd err [FacetDoc]
moreLikeWith cId o l order ft priors = do

  docs_test  <- filter (\(FacetDoc _ _ _ _ f _ _) -> f == Just 1)
            <$> runViewDocuments cId False o Nothing order Nothing Nothing

  let results = map fst
       $ filter ((==) (Just $ not $ fav2bool ft) . snd)
       $ map (\f -> (f, detectDefaultWithPriors text priors f)) docs_test

  pure $ List.take (getLimit $ maybe 10 identity l) results

---------------------------------------------------------------------------
fav2bool :: FavOrTrash -> Bool
fav2bool ft = if (==) ft IsFav then True else False


text :: FacetDoc -> Text
text (FacetDoc _ _ _ h _ _ _)  = title <> "" <> Text.take 100 abstr
  where
    title = maybe "" identity (_hd_title    h)
    abstr = maybe "" identity (_hd_abstract h)

---------------------------------------------------------------------------

{-
apply :: (FlowCmdM env e m) => FavOrTrash -> CorpusId -> [NodeId] -> m [Int]
apply favTrash cId ns = case favTrash of
      IsFav   -> nodeNodesCategory $ map (\n -> (cId, n, 2)) ns
      IsTrash -> nodeNodesCategory $ map (\n -> (cId, n, 0)) ns

moreLikeAndApply :: FlowCmdM DevEnv GargError m => FavOrTrash -> CorpusId -> m [Int]
moreLikeAndApply ft cId = do
  priors <- getPriors ft cId
  moreLikeWithAndApply priors ft cId

moreLikeWithAndApply :: FlowCmdM DevEnv GargError m => Events Bool -> FavOrTrash -> CorpusId -> m [Int]
moreLikeWithAndApply priors ft cId = do
  ids <- map facetDoc_id <$> moreLikeWith cId ft priors
  apply ft cId ids
-}
