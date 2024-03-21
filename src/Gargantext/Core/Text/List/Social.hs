{-|
Module      : Gargantext.Core.Text.List.Social
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.Core.Text.List.Social
  where

import Control.Lens (view)
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Pool ( withResource )
import Data.Swagger ( ToSchema(..), genericDeclareNamedSchema, defaultSchemaOptions )
import Data.Text qualified as T
import Data.Vector qualified as V
import Gargantext.API.Ngrams.Types (NgramsTerm, NgramsPatch)
import Gargantext.Core.NodeStory (getNodesArchiveHistory)
import Gargantext.Core.Text.List.Social.Find (findListsId)
import Gargantext.Core.Text.List.Social.Patch (addScorePatches)
import Gargantext.Core.Text.List.Social.Prelude (FlowCont, FlowListScores)
import Gargantext.Core.Text.Ngrams (NgramsType)
import Gargantext.Core.Types.Individu (User)
import Gargantext.Database.Admin.Types.Node (ListId)
import Gargantext.Database.Prelude (DBCmd, connPool)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Tree (NodeMode(Private), HasTreeError)
import Gargantext.Prelude
import Test.QuickCheck ( Arbitrary(arbitrary), oneof, arbitraryBoundedEnum )
import Web.Internal.HttpApiData (ToHttpApiData, FromHttpApiData, parseUrlPiece, toUrlPiece)
------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Main parameters

-- | FlowSocialListPriority
-- Sociological assumption: either private or others (public) first
-- This parameter depends on the user choice

data FlowSocialListWith = FlowSocialListWithPriority { fslw_priority :: FlowSocialListPriority }
                        | FlowSocialListWithLists    { fslw_lists    :: [ListId]               }
                        | NoList                     { makeList      :: Bool                   }

  deriving (Eq, Show, Generic)
instance FromJSON FlowSocialListWith where
  parseJSON (Object v) = do
    typ :: T.Text <- v .: "type"
    value <- v .:? "value" .!= []
    case typ of
      "MyListsFirst"     -> pure $ FlowSocialListWithPriority { fslw_priority = MySelfFirst }
      "OtherListsFirst"  -> pure $ FlowSocialListWithPriority { fslw_priority = OthersFirst }
      "OthersFirst"      -> pure $ FlowSocialListWithPriority { fslw_priority = OthersFirst }
      "SelectedLists"    -> pure $ FlowSocialListWithLists    { fslw_lists    = value       }
      "NoList"           -> do
        mkList <- v .: "makeList"
        pure $ NoList mkList
      _                  -> pure $ FlowSocialListWithPriority { fslw_priority = MySelfFirst }
  parseJSON _ = mzero
instance ToJSON FlowSocialListWith where
  toJSON (FlowSocialListWithPriority { fslw_priority = MySelfFirst }) = object [ ("type", String "MyListsFirst") ]
  toJSON (FlowSocialListWithPriority { fslw_priority = OthersFirst }) = object [ ("type", String "OthersFirst") ]
  toJSON (NoList v) = object [ ("type", String "NoList"), ("makeList", toJSON v) ]
  toJSON (FlowSocialListWithLists { fslw_lists = ids }) =
    object [ ("type", String "SelectedLists")
           , ("value", Array $ V.fromList (map toJSON ids)) ]

instance Arbitrary FlowSocialListWith where
  arbitrary = oneof [
      FlowSocialListWithPriority <$> arbitrary
    , FlowSocialListWithLists <$> arbitrary
    , NoList <$> arbitrary
    ]

instance ToSchema FlowSocialListWith where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
instance FromHttpApiData FlowSocialListWith
  where
    parseUrlPiece "My lists first"    = pure $ FlowSocialListWithPriority { fslw_priority = MySelfFirst }
    parseUrlPiece "Others lists first" = pure $ FlowSocialListWithPriority { fslw_priority = OthersFirst }
    parseUrlPiece "NoList"          = pure $ NoList True
    parseUrlPiece x                 = panicTrace $ "[G.C.T.L.Social] TODO FromHttpApiData FlowSocialListWith error: " <> (show x)

instance ToHttpApiData   FlowSocialListWith where
    toUrlPiece (FlowSocialListWithPriority  MySelfFirst) = "MySelfFirst"
    toUrlPiece (FlowSocialListWithPriority  OthersFirst) = "OtherListsFirst"
    toUrlPiece (NoList _) = "NoList"
    toUrlPiece (FlowSocialListWithLists _)  = panicTrace "[G.C.T.L.Social] TODO ToHttpApiData FlowSocialListWith"

data FlowSocialListPriority = MySelfFirst | OthersFirst
  deriving (Eq, Show, Generic, Enum, Bounded)
instance ToSchema FlowSocialListPriority where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance Arbitrary FlowSocialListPriority where
  arbitrary = arbitraryBoundedEnum

flowSocialListPriority :: FlowSocialListPriority -> [NodeMode]
flowSocialListPriority MySelfFirst = [Private{-, Shared, Public -}]
flowSocialListPriority OthersFirst = reverse $ flowSocialListPriority MySelfFirst
{-
-- | We keep the parents for all ngrams but terms
keepAllParents :: NgramsType -> KeepAllParents
keepAllParents NgramsTerms = KeepAllParents False
keepAllParents _           = KeepAllParents True
-}

------------------------------------------------------------------------
flowSocialList :: ( HasNodeError err
                  , HasTreeError err
                  )
               => Maybe FlowSocialListWith
               -> User
               -> NgramsType
               -> FlowCont NgramsTerm FlowListScores
               -> DBCmd err (FlowCont NgramsTerm FlowListScores)
flowSocialList Nothing u = flowSocialList' MySelfFirst u
flowSocialList (Just (FlowSocialListWithPriority p))  u = flowSocialList' p u
flowSocialList (Just (FlowSocialListWithLists    ls)) _ = getHistoryScores ls
flowSocialList (Just (NoList _))                     _u = panicTrace "[G.C.T.L.Social] Should not be executed"

flowSocialList' :: ( HasNodeError err
                   , HasTreeError err
                   )
                  => FlowSocialListPriority
                  -> User -> NgramsType
                  -> FlowCont NgramsTerm FlowListScores
                  -> DBCmd err (FlowCont NgramsTerm FlowListScores)
flowSocialList' flowPriority user nt flc =
  mconcat <$> mapM (flowSocialListByMode'   user nt flc)
                   (flowSocialListPriority flowPriority)
    where

      flowSocialListByMode' :: ( HasNodeError err
                               , HasTreeError err
                               )
                            => User -> NgramsType
                            -> FlowCont NgramsTerm FlowListScores
                            -> NodeMode
                            -> DBCmd err (FlowCont NgramsTerm FlowListScores)
      flowSocialListByMode' user' nt' flc' mode =
            findListsId user' mode
        >>= flowSocialListByModeWith nt' flc'


      flowSocialListByModeWith :: ( HasNodeError err
                                  , HasTreeError err
                                  )
                               => NgramsType
                               -> FlowCont NgramsTerm FlowListScores
                               -> [ListId]
                               -> DBCmd err (FlowCont NgramsTerm FlowListScores)
      flowSocialListByModeWith nt'' flc'' listes =
        getHistoryScores listes nt'' flc''


-----------------------------------------------------------------
getHistoryScores :: ( HasNodeError     err
                    , HasTreeError     err
                    )
                 => [ListId]
                 -> NgramsType
                 -> FlowCont NgramsTerm FlowListScores
                 -> DBCmd err (FlowCont NgramsTerm FlowListScores)
getHistoryScores lists nt fl =
  addScorePatches nt lists fl <$> getHistory [nt] lists


getHistory :: ( HasNodeError err
              , HasTreeError err
              )
         => [NgramsType]
         -> [ListId]
         -> DBCmd err (Map ListId (Map NgramsType [HashMap NgramsTerm NgramsPatch]))
getHistory types listsId = do
  pool <- view connPool
  nsp  <- liftBase $ withResource pool $ \c -> getNodesArchiveHistory c listsId
  pure $ Map.map (Map.filterWithKey (\k _ -> List.elem k types))
       $ Map.filterWithKey (\k _ -> List.elem k listsId)
       $ Map.fromListWith (Map.unionWith (<>)) nsp
