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
import Control.Applicative ((<|>))

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH (deriveJSON)
import Data.Monoid
import Data.Swagger
import Data.Text    (Text)
import Data.Time.Clock.POSIX  (POSIXTime)
import qualified Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Instances.Text()
import Prelude (Either(..))

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
    , _gd_data          :: GraphDataData
    } deriving (Show, Eq, Generic)

data GraphDataData =
  GraphDataData {
    _gdd_bb                :: Text
  , _gdd_color             :: Text
  , _gdd_fontsize          :: Text
  , _gdd_label             :: Text
  , _gdd_labelloc          :: Text
  , _gdd_lheight           :: Text
  , _gdd_lp                :: Text
  , _gdd_lwidth            :: Text
  , _gdd_name              :: Text
  , _gdd_nodesep           :: Text
  , _gdd_overlap           :: Text
  , _gdd_phyloBranches     :: Text
  , _gdd_phyloDocs         :: Text
  , _gdd_phyloFoundations  :: Text
  , _gdd_phyloGroups       :: Text
  , _gdd_phyloPeriods      :: Text
  , _gdd_phyloSources      :: Text
  , _gdd_phyloTerms        :: Text
  , _gdd_phyloTimeScale    :: Text
  , _gdd_rank              :: Text
  , _gdd_ranksep           :: Text
  , _gdd_ratio             :: Text
  , _gdd_splines           :: Text
  , _gdd_style             :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON GraphDataData where
  toJSON GraphDataData{..} = object [
      "bb"               .= _gdd_bb
    , "color"            .= _gdd_color
    , "fontsize"         .= _gdd_fontsize
    , "label"            .= _gdd_label
    , "labelloc"         .= _gdd_labelloc
    , "lheight"          .= _gdd_lheight
    , "lp"               .= _gdd_lp
    , "lwidth"           .= _gdd_lwidth
    , "name"             .= _gdd_name
    , "nodesep"          .= _gdd_nodesep
    , "overlap"          .= _gdd_overlap
    , "phyloBranches"    .= _gdd_phyloBranches
    , "phyloDocs"        .= _gdd_phyloDocs
    , "phyloFoundations" .= _gdd_phyloFoundations
    , "phyloGroups"      .= _gdd_phyloGroups
    , "phyloPeriods"     .= _gdd_phyloPeriods
    , "phyloSources"     .= _gdd_phyloSources
    , "phyloTerms"       .= _gdd_phyloTerms
    , "phyloTimeScale"   .= _gdd_phyloTimeScale
    , "rank"             .= _gdd_rank
    , "ranksep"          .= _gdd_ranksep
    , "ratio"            .= _gdd_ratio
    , "splines"          .= _gdd_splines
    , "style"            .= _gdd_style
    ]

instance FromJSON GraphDataData where
  parseJSON = withObject "GraphDataData" $ \o -> do
    _gdd_bb <- o               .: "bb"
    _gdd_color <- o            .: "color"
    _gdd_fontsize <- o         .: "fontsize"
    _gdd_label <- o            .: "label"
    _gdd_labelloc <- o         .: "labelloc"
    _gdd_lheight <- o          .: "lheight"
    _gdd_lp <- o               .: "lp"
    _gdd_lwidth <- o           .: "lwidth"
    _gdd_name <- o             .: "name"
    _gdd_nodesep <- o          .: "nodesep"
    _gdd_overlap <- o          .: "overlap"
    _gdd_phyloBranches <- o    .: "phyloBranches"
    _gdd_phyloDocs <- o        .: "phyloDocs"
    _gdd_phyloFoundations <- o .: "phyloFoundations"
    _gdd_phyloGroups <- o      .: "phyloGroups"
    _gdd_phyloPeriods <- o     .: "phyloPeriods"
    _gdd_phyloSources <- o     .: "phyloSources"
    _gdd_phyloTerms <- o       .: "phyloTerms"
    _gdd_phyloTimeScale <- o   .: "phyloTimeScale"
    _gdd_rank <- o             .: "rank"
    _gdd_ranksep <- o          .: "ranksep"
    _gdd_ratio <- o            .: "ratio"
    _gdd_splines <- o          .: "splines"
    _gdd_style <- o            .: "style"
    pure $ GraphDataData{..}


-- temp placeholder.
data ObjectData =
    GroupToNode  !GvId !NodeCommonData !GroupToNodeData
  | BranchToNode !GvId !NodeCommonData !BranchToNodeData
  | PeriodToNode !GvId !NodeCommonData !PeriodToNodeData
  | Layer        !GvId !GraphDataData  !LayerData
  deriving (Show, Eq, Generic)

instance ToJSON ObjectData where
  toJSON = \case
    GroupToNode gvid commonData nodeTypeData
      -> mkObject gvid (Left commonData) nodeTypeData
    BranchToNode gvid commonData nodeTypeData
      -> mkObject gvid (Left commonData) nodeTypeData
    PeriodToNode gvid commonData nodeTypeData
      -> mkObject gvid (Left commonData) nodeTypeData
    Layer gvid graphData nodeTypeData
      -> mkObject gvid (Right graphData) nodeTypeData

instance FromJSON ObjectData where
  parseJSON = withObject "ObjectData" $ \o -> do
    _gvid        <- o .: "_gvid"
    -- try to parse the graph data first. If we succeed, then we are dealing with
    -- the 'Layer', otherwise we the rest, but for the rest we can avoid re-parsing
    -- the 'NodeCommonData' every time.
    case parseMaybe @_ @GraphDataData parseJSON (Object o) of
      Nothing
        -> do commonData <- parseJSON (Object o)
              ((GroupToNode  <$> pure _gvid <*> pure commonData <*> parseJSON (Object o)) <|>
               (BranchToNode <$> pure _gvid <*> pure commonData <*> parseJSON (Object o)) <|>
               (PeriodToNode <$> pure _gvid <*> pure commonData <*> parseJSON (Object o)))
      Just gd
        -> Layer <$> pure _gvid <*> pure gd <*> parseJSON (Object o)


mkObject :: ToJSON a => GvId -> Either NodeCommonData GraphDataData -> a -> Value
mkObject gvid commonData objectTypeData =
  let commonDataJSON   = either toJSON toJSON commonData
      objectTypeDataJSON = toJSON objectTypeData
      header           = object $ [ "_gvid"    .= toJSON gvid ]
  in case (commonDataJSON, objectTypeDataJSON, header) of
    (Object hdr, Object cdJSON, Object etDataJSON)
      -> Object $ hdr <> cdJSON <> etDataJSON
    _ -> panic "[Gargantext.Core.Types.Phylo.mkObject] impossible: commonData, header or objectTypeDataJSON didn't convert back to JSON Object."

data GroupToNodeData
  = GroupToNodeData
    { _gtn_bId        :: Text
    , _gtn_branchId   :: Text
    , _gtn_fontname   :: Text
    , _gtn_foundation :: Text
    , _gtn_frequence  :: Text
    , _gtn_from       :: Text
    , _gtn_lbl        :: Text
    , _gtn_penwidth   :: Text
    , _gtn_role       :: Text
    , _gtn_seaLvl     :: Maybe Text
    , _gtn_source     :: Text
    , _gtn_strFrom    :: Maybe Text
    , _gtn_strTo      :: Maybe Text
    , _gtn_support    :: Text
    , _gtn_to         :: Text
    , _gtn_weight     :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON GroupToNodeData where
  toJSON GroupToNodeData{..} = object [
      "bId"        .= _gtn_bId
    , "branchId"   .= _gtn_branchId
    , "fontname"   .= _gtn_fontname
    , "foundation" .= _gtn_foundation
    , "frequence"  .= _gtn_frequence
    , "from"       .= _gtn_from
    , "lbl"        .= _gtn_lbl
    , "penwidth"   .= _gtn_penwidth
    , "role"       .= _gtn_role
    , "seaLvl"     .= _gtn_seaLvl
    , "source"     .= _gtn_source
    , "strFrom"    .= _gtn_strFrom
    , "strTo"      .= _gtn_strTo
    , "support"    .= _gtn_support
    , "to"         .= _gtn_to
    , "weight"     .= _gtn_weight
    ]

instance FromJSON GroupToNodeData where
  parseJSON = withObject "GroupToNodeData" $ \o -> do
    _gtn_bId        <- o .: "bId"
    _gtn_branchId   <- o .: "branchId"
    _gtn_fontname   <- o .: "fontname"
    _gtn_foundation <- o .: "foundation"
    _gtn_frequence  <- o .: "frequence"
    _gtn_from       <- o .: "from"
    _gtn_lbl        <- o .: "lbl"
    _gtn_penwidth   <- o .: "penwidth"
    _gtn_role       <- o .: "role"
    _gtn_seaLvl     <- o .:? "seaLvl"
    _gtn_source     <- o .: "source"
    _gtn_strFrom    <- o .:? "strFrom"
    _gtn_strTo      <- o .:? "strTo"
    _gtn_support    <- o .: "support"
    _gtn_to         <- o .: "to"
    _gtn_weight     <- o .: "weight"
    pure $ GroupToNodeData{..}


data BranchToNodeData
  = BranchToNodeData
    { _btn_age       :: Text
    , _btn_bId       :: Text
    , _btn_birth     :: Text
    , _btn_branchId  :: Text
    , _btn_branch_x  :: Text
    , _btn_branch_y  :: Text
    , _btn_fillcolor :: Text
    , _btn_fontname  :: Text
    , _btn_fontsize  :: Text
    , _btn_size      :: Text
    , _btn_style     :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON BranchToNodeData where
  toJSON BranchToNodeData{..} = object [
      "age"       .= _btn_age
    , "bId"       .= _btn_bId
    , "birth"     .= _btn_birth
    , "branchId"  .= _btn_branchId
    , "branch_x"  .= _btn_branch_x
    , "branch_y"  .= _btn_branch_y
    , "fillcolor" .= _btn_fillcolor
    , "fontname"  .= _btn_fontname
    , "fontsize"  .= _btn_fontsize
    , "size"      .= _btn_size
    , "style"     .= _btn_style
    ]

instance FromJSON BranchToNodeData where
  parseJSON = withObject "BranchToNodeData" $ \o -> do
    _btn_age       <- o .: "age"
    _btn_bId       <- o .: "bId"
    _btn_birth     <- o .: "birth"
    _btn_branchId  <- o .: "branchId"
    _btn_branch_x  <- o .: "branch_x"
    _btn_branch_y  <- o .: "branch_y"
    _btn_fillcolor <- o .: "fillcolor"
    _btn_fontname  <- o .: "fontname"
    _btn_fontsize  <- o .: "fontsize"
    _btn_size      <- o .: "size"
    _btn_style     <- o .: "style"
    pure $ BranchToNodeData{..}

data PeriodToNodeData
  = PeriodToNodeData
    { _ptn_fontsize :: Text
    , _ptn_from     :: Text
    , _ptn_strFrom  :: Maybe Text
    , _ptn_strTo    :: Maybe Text
    , _ptn_to       :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON PeriodToNodeData where
  toJSON PeriodToNodeData{..} = object [
      "fontsize" .= _ptn_fontsize
    , "from"     .= _ptn_from
    , "strFrom"  .= _ptn_strFrom
    , "strTo"    .= _ptn_strTo
    , "to"       .= _ptn_to
    ]

instance FromJSON PeriodToNodeData where
  parseJSON = withObject "PeriodToNodeData" $ \o -> do
    _ptn_fontsize <- o .:  "fontsize"
    _ptn_from     <- o .:  "from"
    _ptn_strFrom  <- o .:? "strFrom"
    _ptn_strTo    <- o .:? "strTo"
    _ptn_to       <- o .:  "to"
    pure $ PeriodToNodeData{..}

data LayerData
  = LayerData {
    _ld_nodes :: [Int]
    } deriving (Show, Eq, Generic)

instance ToJSON LayerData where
  toJSON LayerData{..} = object [
      "nodes" .= toJSON _ld_nodes
    ]

instance FromJSON LayerData where
  parseJSON = withObject "LayerData" $ \o -> do
    _ld_nodes <- o .: "nodes"
    pure $ LayerData{..}

data NodeCommonData =
  NodeCommonData {
    _nd_height   :: !Text
  , _nd_label    :: !Text
  , _nd_name     :: !Text
  , _nd_nodeType :: !Text
  , _nd_pos      :: !Text
  , _nd_shape    :: !Text
  , _nd_width    :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON NodeCommonData where
  toJSON NodeCommonData{..} = object [
      "height"   .= _nd_height
    , "label"    .= _nd_label
    , "name"     .= _nd_name
    , "nodeType" .= _nd_nodeType
    , "pos"      .= _nd_pos
    , "shape"    .= _nd_shape
    , "width"    .= _nd_width
    ]

instance FromJSON NodeCommonData where
  parseJSON = withObject "NodeCommonData" $ \o -> do
    _nd_height   <- o .: "height"
    _nd_label    <- o .: "label"
    _nd_name     <- o .: "name"
    _nd_nodeType <- o .: "nodeType"
    _nd_pos      <- o .: "pos"
    _nd_shape    <- o .: "shape"
    _nd_width    <- o .: "width"
    pure $ NodeCommonData{..}

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
  toJSON = mkGraphData

mkGraphData :: GraphData -> Value
mkGraphData GraphData{..} =
  let hdrJSON = object [ "_subgraph_cnt" .= _gd__subgraph_cnt
                       , "directed"      .= _gd_directed
                       , "edges"         .= _gd_edges
                       , "objects"       .= _gd_objects
                       , "strict"        .= _gd_strict
                       ]
      datJSON = toJSON _gd_data
  in case (hdrJSON, datJSON) of
    (Object a, Object b) -> Object $ a <> b
    _ -> panic "[Gargantext.Core.Types.Phylo.mkGraphData] impossible: header or data didn't convert back to JSON Object."

instance FromJSON GraphData where
  parseJSON = withObject "GraphData" $ \o -> do
    _gd__subgraph_cnt <- o .: "_subgraph_cnt"
    _gd_directed      <- o .: "directed"
    _gd_edges         <- o .: "edges"
    _gd_objects       <- o .: "objects"
    _gd_strict        <- o .: "strict"
    _gd_data          <- parseJSON (Object o)
    pure GraphData{..}

instance ToJSON GvId where
  toJSON GvId{..} = toJSON _GvId
instance FromJSON GvId where
  parseJSON v = GvId <$> parseJSON v

instance ToJSON EdgeData where
  toJSON = \case
    GroupToAncestor gvid commonData edgeTypeData
      -> mkEdge "ancestorLink" gvid commonData edgeTypeData
    GroupToGroup gvid commonData edgeTypeData
      -> mkEdge "link" gvid commonData edgeTypeData
    BranchToGroup gvid commonData edgeTypeData
      -> mkEdge "branchLink" gvid commonData edgeTypeData

mkEdge :: ToJSON a => Text -> GvId -> EdgeCommonData -> a -> Value
mkEdge edgeType gvid commonData edgeTypeData =
  let commonDataJSON   = toJSON commonData
      edgeTypeDataJSON = toJSON edgeTypeData
      header           = object $ [ "edgeType" .= toJSON edgeType
                                  , "_gvid"    .= toJSON gvid
                                  ]
  in case (commonDataJSON, edgeTypeDataJSON, header) of
    (Object hdr, Object cdJSON, Object etDataJSON)
      -> Object $ hdr <> cdJSON <> etDataJSON
    _ -> panic "[Gargantext.Core.Types.Phylo.mkEdge] impossible: commonData, header or edgeTypeDataJSON didn't convert back to JSON Object."


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
instance ToSchema GraphDataData where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_gdd_")
instance ToSchema GraphData where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_gd_")

-- | Arbitrary instances
instance Arbitrary LayerData where
  arbitrary = LayerData <$> arbitrary
instance Arbitrary NodeCommonData where
  arbitrary = NodeCommonData <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary
instance Arbitrary GroupToNodeData where
  arbitrary = GroupToNodeData <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
instance Arbitrary BranchToNodeData where
  arbitrary = BranchToNodeData <$> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
instance Arbitrary PeriodToNodeData where
  arbitrary = PeriodToNodeData <$> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
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
  arbitrary = oneof [ GroupToNode  <$> arbitrary <*> arbitrary <*> arbitrary
                    , BranchToNode <$> arbitrary <*> arbitrary <*> arbitrary
                    , PeriodToNode <$> arbitrary <*> arbitrary <*> arbitrary
                    , Layer        <$> arbitrary <*> arbitrary <*> arbitrary
                    ]
instance Arbitrary GvId where
  arbitrary = GvId <$> arbitrary
instance Arbitrary EdgeData where
  arbitrary = oneof [ GroupToAncestor <$> arbitrary <*> arbitrary <*> arbitrary
                    , GroupToGroup    <$> arbitrary <*> arbitrary <*> arbitrary
                    , BranchToGroup   <$> arbitrary <*> arbitrary <*> arbitrary
                    ]
instance Arbitrary GraphData where
  arbitrary = GraphData <$> arbitrary <*> arbitrary <*> vectorOf 10 arbitrary <*> vectorOf 10 arbitrary
                        <*> arbitrary <*> arbitrary
instance Arbitrary GraphDataData where
  arbitrary = GraphDataData <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                            <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
