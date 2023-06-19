{-|
Module      : Gargantext.Types.Phylo
Description : Main Types for Phylomemy
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Specifications of Phylomemy format.

Phylomemy can be described as a Temporal Graph with different scale of
granularity of group of ngrams (terms and multi-terms).

The main type is Phylo which is synonym of Phylomemy (only difference is
the number of chars).

Phylomemy was first described in Chavalarias, D., Cointet, J.-P., 2013. Phylomemetic patterns in science evolution—the rise and fall of scientific fields. PloS one 8, e54847.
.
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

module Gargantext.Core.Types.Phylo where

import Control.Monad.Fail (fail)
import Control.Lens (makeLenses)

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Monoid
import Data.Swagger
import Data.Text    (Text)
import Data.Time.Clock.POSIX  (POSIXTime)
import qualified Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Instances.Text()

import GHC.Generics (Generic)

import Gargantext.Prelude
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)

------------------------------------------------------------------------
-- | Phylo datatype descriptor of a phylomemy
-- Duration : time Segment of the whole phylomemy in UTCTime format (start,end)
-- Ngrams   : list of all (possible) terms contained in the phylomemy (with their id)
-- Steps    : list of all steps to build the phylomemy
data Phylo = Phylo { _phylo_Duration :: (Start, End)
                   , _phylo_Ngrams   :: [Ngram]
                   , _phylo_Periods  :: [PhyloPeriod]
                   } deriving (Generic)

-- | UTCTime in seconds since UNIX epoch
type Start   = POSIXTime
type End     = POSIXTime

-- | Indexed Ngram
type Ngram   = (NgramId, Text)
type NgramId = Int

-- | PhyloStep : steps of phylomemy on temporal axis
-- Period: tuple (start date, end date) of the step of the phylomemy
-- Levels: levels of granularity
data PhyloPeriod = PhyloPeriod { _phylo_PeriodId     :: PhyloPeriodId
                               , _phylo_PeriodLevels :: [PhyloLevel]
                               } deriving (Generic)

type PhyloPeriodId = (Start, End)

-- | PhyloLevel : levels of phylomemy on level axis
-- Levels description:
-- Level -1: Ngram equals itself         (by identity) == _phylo_Ngrams
-- Level  0: Group of synonyms           (by stems + by qualitative expert meaning)
-- Level  1: First level of clustering
-- Level  N: Nth   level of clustering
data PhyloLevel = PhyloLevel { _phylo_LevelId     :: PhyloLevelId
                             , _phylo_LevelGroups :: [PhyloGroup]
                             } deriving (Generic)

type PhyloLevelId = (PhyloPeriodId, Int)

-- | PhyloGroup : group of ngrams at each level and step
-- Label : maybe has a label as text
-- Ngrams: set of terms that build the group
-- Period Parents|Childs: weighted link to Parents|Childs (Temporal Period   axis)
-- Level  Parents|Childs: weighted link to Parents|Childs (Level Granularity axis)
data PhyloGroup = PhyloGroup { _phylo_GroupId            :: PhyloGroupId
                             , _phylo_GroupLabel         :: Maybe Text
                             , _phylo_GroupNgrams        :: [NgramId]
                   
                             , _phylo_GroupPeriodParents :: [Edge]
                             , _phylo_GroupPeriodChilds  :: [Edge]
                   
                             , _phylo_GroupLevelParents  :: [Edge]
                             , _phylo_GroupLevelChilds   :: [Edge]
                             } deriving (Generic)

type PhyloGroupId = (PhyloLevelId, Int)
type Edge         = (PhyloGroupId, Weight)
type Weight       = Double

------------------------------------------------------------------------
-- | Phylo 'GraphData' datatype descriptor. It must be isomorphic to
-- the 'GraphData' type of the purecript frontend.

data GraphData =
  GraphData {
      _gd__subgraph_cnt :: Int
    , _gd_directed      :: Bool
    , _gd_edges         :: [EdgeData]
    , _gd_objects       :: [ObjectData]
    , _gd_strict        :: Bool
    } deriving (Show, Eq, Generic)

-- temp placeholder.
newtype ObjectData = ObjectData { _ObjectData :: Value }
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

data EdgeCommonData =
  EdgeCommonData {
    _ed_color :: !Text
  , _ed_head  :: !Int
  , _ed_pos   :: !Text
  , _ed_tail  :: !Int
  , _ed_width :: !Text
  } deriving (Show, Eq, Generic)

newtype GvId = GvId { _GvId :: Int }
  deriving (Show, Eq, Generic)

data EdgeData
  = GroupToAncestor !GvId !EdgeCommonData !GroupToAncestorData
  | GroupToGroup    !GvId !EdgeCommonData !GroupToGroupData
  | BranchToGroup   !GvId !EdgeCommonData !BranchToGroupData
  deriving (Show, Eq, Generic)

data GroupToAncestorData
  = GroupToAncestorData
    { _gta_arrowhead :: !Text
    , _gta_lbl       :: !Text
    , _gta_penwidth  :: !Text
    , _gta_style     :: !Text
    } deriving (Show, Eq, Generic)

data GroupToGroupData
  = GroupToGroupData
    { _gtg_constraint :: !Text
    , _gtg_lbl        :: !Text
    , _gtg_penwidth   :: !Text
    } deriving (Show, Eq, Generic)

data BranchToGroupData
  = BranchToGroupData
    { _btg_arrowhead :: !Text
    , _btg_style :: Maybe Text
    } deriving (Show, Eq, Generic)

-- | Lenses
makeLenses ''Phylo
makeLenses ''PhyloPeriod
makeLenses ''PhyloLevel
makeLenses ''PhyloGroup

-- | JSON instances
$(deriveJSON (unPrefix "_phylo_"       ) ''Phylo       )
$(deriveJSON (unPrefix "_phylo_Period" ) ''PhyloPeriod )
$(deriveJSON (unPrefix "_phylo_Level"  ) ''PhyloLevel  )
$(deriveJSON (unPrefix "_phylo_Group"  ) ''PhyloGroup  )

instance ToJSON GraphData where
  toJSON GraphData{..} = object
    [ "_subgraph_cnt" .= _gd__subgraph_cnt
    , "directed"      .= _gd_directed
    , "edges"         .= _gd_edges
    , "objects"       .= _gd_objects
    , "strict"        .= _gd_strict
    ]

instance FromJSON GraphData where
  parseJSON = withObject "GraphData" $ \o -> do
    _gd__subgraph_cnt <- o .: "_subgraph_cnt"
    _gd_directed      <- o .: "directed"
    _gd_edges         <- o .: "edges"
    _gd_objects       <- o .: "objects"
    _gd_strict        <- o .: "strict"
    pure GraphData{..}

instance ToJSON GvId where
  toJSON GvId{..} = toJSON _GvId
instance FromJSON GvId where
  parseJSON v = GvId <$> parseJSON v

instance ToJSON EdgeData where
  toJSON = \case
    GroupToAncestor gvid commonData edgeTypeData
      -> mkNode "ancestorLink" gvid commonData edgeTypeData
    GroupToGroup gvid commonData edgeTypeData
      -> mkNode "link" gvid commonData edgeTypeData
    BranchToGroup gvid commonData edgeTypeData
      -> mkNode "branchLink" gvid commonData edgeTypeData

mkNode :: ToJSON a => Text -> GvId -> EdgeCommonData -> a -> Value
mkNode edgeType gvid commonData edgeTypeData =
  let commonDataJSON   = toJSON commonData
      edgeTypeDataJSON = toJSON edgeTypeData
      header           = object $ [ "edgeType" .= toJSON edgeType
                                  , "_gvid"    .= toJSON gvid
                                  ]
  in case (commonDataJSON, edgeTypeDataJSON, header) of
    (Object hdr, Object cdJSON, Object etDataJSON)
      -> Object $ hdr <> cdJSON <> etDataJSON
    _ -> panic "[Gargantext.Core.Types.Phylo.mkNode] impossible: commonData, header or edgeTypeDataJSON didn't convert back to JSON Object."


instance FromJSON EdgeData where
  parseJSON = withObject "EdgeData" $ \o -> do
    edgeType  <- o .: "edgeType"
    gvid      <- o .: "_gvid"
    _ed_color <- o .: "color"
    _ed_head  <- o .: "head"
    _ed_pos   <- o .: "pos"
    _ed_tail  <- o .: "tail"
    _ed_width <- o .: "width"
    case (edgeType :: Text) of
      "ancestorLink" -> GroupToAncestor <$> pure gvid <*> pure EdgeCommonData{..} <*> parseJSON (Object o)
      "link"         -> GroupToGroup    <$> pure gvid <*> pure EdgeCommonData{..} <*> parseJSON (Object o)
      "branchLink"   -> BranchToGroup   <$> pure gvid <*> pure EdgeCommonData{..} <*> parseJSON (Object o)
      _              -> fail $ "EdgeData: unrecognised edgeType for Phylo graph: " <> T.unpack edgeType

instance ToJSON EdgeCommonData where
  toJSON EdgeCommonData{..} = object
    [ "color" .= _ed_color
    , "head"  .= _ed_head
    , "pos"   .= _ed_pos
    , "tail"  .= _ed_tail
    , "width" .= _ed_width
    ]

instance ToJSON GroupToAncestorData where
  toJSON GroupToAncestorData{..} =
    object [ "arrowhead" .= _gta_arrowhead
           , "lbl"       .= _gta_lbl
           , "penwidth"  .= _gta_penwidth
           , "style"     .= _gta_style
           ]

instance FromJSON GroupToAncestorData where
  parseJSON = withObject "GroupToAncestorData" $ \o -> do
    _gta_arrowhead <- o .: "arrowhead"
    _gta_lbl       <- o .: "lbl"
    _gta_penwidth  <- o .: "penwidth"
    _gta_style     <- o .: "style"
    pure GroupToAncestorData{..}

instance ToJSON GroupToGroupData where
  toJSON GroupToGroupData{..} =
    object [ "constraint" .= _gtg_constraint
           , "lbl"        .= _gtg_lbl
           , "penwidth"   .= _gtg_penwidth
           ]

instance FromJSON GroupToGroupData where
  parseJSON = withObject "BranchToGroupData" $ \o -> do
    _gtg_constraint <- o .: "constraint"
    _gtg_lbl        <- o .: "lbl"
    _gtg_penwidth   <- o .: "penwidth"
    pure GroupToGroupData{..}

instance ToJSON BranchToGroupData where
  toJSON BranchToGroupData{..} =
    object [ "arrowhead" .= _btg_arrowhead
           , "style"     .= _btg_style
           ]

instance FromJSON BranchToGroupData where
  parseJSON = withObject "BranchToGroupData" $ \o -> do
    _btg_arrowhead <- o .:  "arrowhead"
    _btg_style     <- o .:? "style"
    pure BranchToGroupData{..}


-- | ToSchema instances
instance ToSchema Phylo where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_phylo_")
instance ToSchema PhyloPeriod where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_phylo_Period")
instance ToSchema PhyloLevel where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_phylo_Level")
instance ToSchema PhyloGroup where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_phylo_Group")
instance ToSchema BranchToGroupData where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_btg_")
instance ToSchema GroupToGroupData where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_gtg_")
instance ToSchema GroupToAncestorData where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_gta_")
instance ToSchema EdgeCommonData where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_ed_")
instance ToSchema ObjectData where
  declareNamedSchema _ = pure $ NamedSchema (Just "ObjectData") $ mempty
instance ToSchema GvId where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
instance ToSchema EdgeData where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
instance ToSchema GraphData where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_gd_")

-- | Arbitrary instances
instance Arbitrary BranchToGroupData where
  arbitrary = BranchToGroupData <$> arbitrary <*> arbitrary
instance Arbitrary GroupToGroupData where
  arbitrary = GroupToGroupData <$> arbitrary
                               <*> arbitrary
                               <*> arbitrary
instance Arbitrary GroupToAncestorData where
  arbitrary = GroupToAncestorData <$> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
instance Arbitrary EdgeCommonData where
  arbitrary = EdgeCommonData <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary
instance Arbitrary ObjectData where
  arbitrary = ObjectData <$> (String <$> arbitrary) -- temporary, it doesn't matter.
instance Arbitrary GvId where
  arbitrary = GvId <$> arbitrary
instance Arbitrary EdgeData where
  arbitrary = oneof [ GroupToAncestor <$> arbitrary <*> arbitrary <*> arbitrary
                    , GroupToGroup    <$> arbitrary <*> arbitrary <*> arbitrary
                    , BranchToGroup   <$> arbitrary <*> arbitrary <*> arbitrary
                    ]
instance Arbitrary GraphData where
  arbitrary = GraphData <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
