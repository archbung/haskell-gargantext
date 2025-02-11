{-|
Module      : Gargantext.Core.Viz.Phylo.API
Description : Phylo API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedLists    #-}   -- allows to write Map and HashMap as lists
{-# LANGUAGE TypeOperators      #-}

module Gargantext.Core.Viz.Phylo.API
  where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as DB
import Data.ByteString.Lazy qualified as DBL
import Data.Swagger
import Data.Text qualified as T
import Gargantext.API.Prelude
import Gargantext.Core.Types (TODO(..))
import Gargantext.Core.Types.Phylo (GraphData(..))
import Gargantext.Core.Viz.LegacyPhylo hiding (Phylo(..))
import Gargantext.Core.Viz.Phylo (PhyloConfig(..), defaultConfig, _phylo_param, _phyloParam_config)
import Gargantext.Core.Viz.Phylo.API.Tools
import Gargantext.Core.Viz.Phylo.Example (phyloCleopatre)
import Gargantext.Core.Viz.Phylo.Legacy.LegacyMain
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node -- (PhyloId, ListId, CorpusId, UserId, NodeId(..))
import Gargantext.Database.Query.Table.Node (getClosestParentIdByType, defaultList)
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Prelude
import Network.HTTP.Media ((//), (/:))
import Prelude qualified
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Web.HttpApiData (readTextData)
import Gargantext.Database.Query.Table.Node.Error

------------------------------------------------------------------------
type PhyloAPI = Summary "Phylo API"
              :> GetPhylo
        --    :<|> PutPhylo
            :<|> PostPhylo


phyloAPI :: PhyloId -> GargServer PhyloAPI
phyloAPI n = getPhylo  n
        :<|> postPhylo n
        -- :<|> putPhylo  n
        -- :<|> deletePhylo  n

newtype SVG = SVG DB.ByteString
--instance Show a => MimeRender PlainText a where mimeRender _ val = cs ("" <> show val)
instance Accept SVG where contentType _ = "SVG" // "image/svg+xml" /: ("charset", "utf-8")
instance MimeRender SVG SVG where mimeRender _ (SVG s) = DBL.fromStrict s
instance MimeUnrender SVG SVG where mimeUnrender _ lbs = Right $ SVG (DBL.toStrict lbs)
instance Prelude.Show SVG where show (SVG a) = show a
instance ToSchema SVG where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy TODO)

------------------------------------------------------------------------
instance ToSchema Value where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy TODO)

------------------------------------------------------------------------

-- | This type is emitted by the backend and the frontend expects to deserialise it
-- as a 'PhyloJSON'. see module 'Gargantext.Components.PhyloExplorer.JSON' of the
-- 'purescript-gargantext' package.
data PhyloData = PhyloData { pd_corpusId :: NodeId
                           , pd_listId   :: NodeId
                           , pd_data     :: GraphData
                           , pd_config   :: PhyloConfig
                           }
  deriving (Generic, Show, Eq)

instance ToJSON PhyloData where
  toJSON PhyloData{..} =
    object [
      "pd_corpusId" .= toJSON pd_corpusId
    , "pd_listId"   .= toJSON pd_listId
    , "pd_data"     .= toJSON pd_data
    , "pd_config"   .= toJSON pd_config
    ]

instance FromJSON PhyloData where
  parseJSON = withObject "PhyloData" $ \o -> do
    pd_corpusId <- o .: "pd_corpusId"
    pd_listId   <- o .: "pd_listId"
    pd_data     <- o .: "pd_data"
    pd_config   <- o .: "pd_config"
    pure $ PhyloData{..}

instance Arbitrary PhyloData where
  arbitrary = PhyloData <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToSchema PhyloData

type GetPhylo =  QueryParam "listId"      ListId
              :> QueryParam "level"       Level
              :> QueryParam "minSizeBranch" MinSizeBranch
   {-           :> QueryParam "filiation"   Filiation
              :> QueryParam "childs"      Bool
              :> QueryParam "depth"       Level
              :> QueryParam "metrics"    [Metric]
              :> QueryParam "periodsInf" Int
              :> QueryParam "periodsSup" Int
              :> QueryParam "minNodes"   Int
              :> QueryParam "taggers"    [Tagger]
              :> QueryParam "sort"       Sort
              :> QueryParam "order"      Order
              :> QueryParam "export"    ExportMode
              :> QueryParam "display"    DisplayMode
              :> QueryParam "verbose"     Bool
    -}
              -- :> Get '[SVG] SVG
              :> Get '[JSON] PhyloData


-- | TODO
-- Add real text processing
-- Fix Filter parameters
-- TODO fix parameters to default config that should be in Node
getPhylo :: PhyloId -> GargServer GetPhylo
getPhylo phyloId lId _level _minSizeBranch = do
  corpusId <- maybe (nodeLookupError $ NodeParentDoesNotExist phyloId) pure
              =<< getClosestParentIdByType phyloId NodeCorpus
  listId   <- case lId of
                Nothing -> defaultList corpusId
                Just ld -> pure ld
  (gd, phyloConfig) <- getPhyloDataJson phyloId
  -- printDebug "getPhylo" theData
  pure $ PhyloData corpusId listId gd phyloConfig



getPhyloDataJson :: PhyloId -> GargNoServer (GraphData, PhyloConfig)
getPhyloDataJson phyloId = do
  maybePhyloData <- getPhyloData phyloId
  let phyloData = fromMaybe phyloCleopatre maybePhyloData
  let phyloConfig = _phyloParam_config $ _phylo_param phyloData
  phyloJson <- liftBase $ phylo2dot2json phyloData
  case parseEither parseJSON phyloJson of
    Left err -> panicTrace $ T.pack $ "[Gargantext.Core.Viz.Phylo.API] getPhyloDataJson: " <> err
    Right gd -> pure (gd, phyloConfig)


-- getPhyloDataSVG phId _lId l msb  = do
  -- let
  --   level = fromMaybe 2 l
  --   branc = fromMaybe 2 msb
  --   maybePhylo = phNode ^. (node_hyperdata . hp_data)

  -- p <- liftBase $ viewPhylo2Svg
  --               $ viewPhylo level branc
  --               $ fromMaybe phyloFromQuery maybePhylo
  -- pure (SVG p)


------------------------------------------------------------------------
type PostPhylo =  QueryParam "listId" ListId
          --     :> ReqBody '[JSON] PhyloQueryBuild
               :> (Post '[JSON] NodeId)

postPhylo :: PhyloId -> GargServer PostPhylo
postPhylo phyloId _lId = do
  -- TODO get Reader settings
  -- s <- ask
  -- let
    -- _vrs = Just ("1" :: Text)
    -- _sft = Just (Software "Gargantext" "4")
    -- _prm = initPhyloParam vrs sft (Just q)
  corpusId <- getClosestParentIdByType phyloId NodeCorpus
  phy <- flowPhyloAPI defaultConfig (fromMaybe (panicTrace "[G.C.V.P.API] no corpus ID found") corpusId) -- params
  -- phyloId <- insertNodes [node NodePhylo "Phylo" (HyperdataPhylo Nothing (Just phy)) (Just corpusId) userId]
  _ <- updateHyperdata phyloId (HyperdataPhylo Nothing (Just phy))
  pure phyloId

------------------------------------------------------------------------
-- | DELETE Phylo == delete a node
------------------------------------------------------------------------
------------------------------------------------------------------------
{-
type PutPhylo = (Put '[JSON] Phylo  )
--putPhylo :: PhyloId -> Maybe ListId -> PhyloQueryBuild -> Phylo
putPhylo :: PhyloId -> GargServer PutPhylo
putPhylo = undefined
-}


-- | Instances
-- instance Arbitrary Phylo             where arbitrary     = elements [phylo]
instance Arbitrary PhyloGroup        where arbitrary     = elements []
-- instance Arbitrary PhyloView         where arbitrary     = elements [phyloView]
-- instance Arbitrary PhyloConfig       where arbitrary     = elements []
instance FromHttpApiData DisplayMode where parseUrlPiece = readTextData
instance FromHttpApiData ExportMode  where parseUrlPiece = readTextData
instance FromHttpApiData Filiation   where parseUrlPiece = readTextData
instance FromHttpApiData Metric      where parseUrlPiece = readTextData
instance FromHttpApiData Order       where parseUrlPiece = readTextData
instance FromHttpApiData Sort        where parseUrlPiece = readTextData
instance FromHttpApiData Tagger      where parseUrlPiece = readTextData
instance FromHttpApiData [Metric]    where parseUrlPiece = readTextData
instance FromHttpApiData [Tagger]    where parseUrlPiece = readTextData
instance ToParamSchema   DisplayMode
instance ToParamSchema   ExportMode
instance ToParamSchema   Filiation
instance ToParamSchema   Tagger
instance ToParamSchema Metric
instance ToParamSchema Order
instance ToParamSchema Sort
instance ToSchema Order
