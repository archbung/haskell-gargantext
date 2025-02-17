{-|
Module      : Gargantext.API.Node.Update
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators      #-}

module Gargantext.API.Node.Update
      where

import Control.Lens (view)
import Data.Aeson
import Data.Set qualified as Set
import Data.Swagger ( ToSchema )
import Gargantext.API.Admin.EnvTypes (GargJob(..), Env)
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Errors.Types ( BackendInternalError )
import Gargantext.API.Metrics qualified as Metrics
import Gargantext.API.Ngrams.Types qualified as NgramsTypes
import Gargantext.API.Prelude (GargM, simuLogs)
import Gargantext.Core.Methods.Similarities (GraphMetric(..))
import Gargantext.Core.NodeStory.Types (HasNodeStory)
import Gargantext.Core.Text.Ngrams (NgramsType(NgramsTerms))
import Gargantext.Core.Types.Main (ListType(..))
import Gargantext.Core.Viz.Graph.API (recomputeGraph)
import Gargantext.Core.Viz.Graph.Tools (PartitionMethod(..), BridgenessMethod(..))
import Gargantext.Core.Viz.Graph.Types (Strength)
import Gargantext.Core.Viz.Phylo (PhyloSubConfigAPI(..), subConfigAPI2config)
import Gargantext.Core.Viz.Phylo.API.Tools (flowPhyloAPI)
import Gargantext.Database.Action.Flow (reIndexWith)
import Gargantext.Database.Action.Flow.Pairing (pairing)
import Gargantext.Database.Action.Metrics (updateNgramsOccurrences, updateContextScore)
import Gargantext.Database.Admin.Types.Hyperdata.Phylo ( HyperdataPhylo(HyperdataPhylo) )
import Gargantext.Database.Admin.Types.Node ( NodeId, NodeType(NodeCorpus, NodeAnnuaire) )
import Gargantext.Database.Query.Table.Node (defaultList, getNode)
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Schema.Node (node_parent_id)
import Gargantext.Prelude
import Gargantext.System.Logging ( MonadLogger )
import Gargantext.Utils.Aeson qualified as GUA
import Gargantext.Utils.Jobs (serveJobsAPI, MonadJobStatus(..))
import Gargantext.Utils.UTCTime (timeMeasured)
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary ( Arbitrary(arbitrary) )

------------------------------------------------------------------------
type API = Summary " Update node according to NodeType params"
         :> AsyncJobs JobLog '[JSON] UpdateNodeParams JobLog

------------------------------------------------------------------------
data UpdateNodeParams = UpdateNodeParamsList  { methodList  :: !Method      }

                      | UpdateNodeParamsGraph { methodGraphMetric        :: !GraphMetric
                                              , methodGraphClustering    :: !PartitionMethod
                                              , methodGraphBridgeness    :: !BridgenessMethod
                                              , methodGraphEdgesStrength :: !Strength
                                              , methodGraphNodeType1     :: !NgramsType
                                              , methodGraphNodeType2     :: !NgramsType
                                              }

                      | UpdateNodeParamsTexts { methodTexts :: !Granularity }

                      | UpdateNodeParamsBoard { methodBoard :: !Charts      }

                      | LinkNodeReq           { nodeType    :: !NodeType
                                              , id          :: !NodeId }

                      | UpdateNodePhylo       { config :: !PhyloSubConfigAPI }
    deriving (Generic)

----------------------------------------------------------------------
data Method = Basic | Advanced | WithModel
    deriving (Generic, Eq, Ord, Enum, Bounded)

----------------------------------------------------------------------
data Granularity = NewNgrams | NewTexts | Both
    deriving (Generic, Eq, Ord, Enum, Bounded)

----------------------------------------------------------------------
data Charts = Sources | Authors | Institutes | Ngrams | All
    deriving (Generic, Eq, Ord, Enum, Bounded)

------------------------------------------------------------------------
api :: NodeId -> ServerT API (GargM Env BackendInternalError)
api nId =
  serveJobsAPI UpdateNodeJob $ \jHandle p ->
    updateNode nId p jHandle

updateNode :: (HasNodeStory env err m
              , HasSettings env
              , MonadJobStatus m
              , MonadLogger m
              )
           => NodeId
           -> UpdateNodeParams
           -> JobHandle m
           -> m ()
updateNode nId (UpdateNodeParamsGraph metric partitionMethod bridgeMethod strength nt1 nt2) jobHandle = do

  markStarted 2 jobHandle
  -- printDebug "Computing graph: " method
  _ <- recomputeGraph nId partitionMethod bridgeMethod (Just metric) (Just strength) nt1 nt2 True
  -- printDebug "Graph computed: " method
  markComplete jobHandle

updateNode nid1 (LinkNodeReq nt nid2) jobHandle = do
  markStarted 2 jobHandle
  _ <- case nt of
    NodeAnnuaire -> pairing nid2 nid1 Nothing -- defaultList
    NodeCorpus   -> pairing nid1 nid2 Nothing -- defaultList
    _            -> panicTrace $ "[G.API.N.Update.updateNode] NodeType not implemented"
                               <> show nt <> " nid1: " <> show nid1 <> " nid2: " <> show nid2

  markComplete jobHandle

-- | `Advanced` to update graphs
updateNode lId (UpdateNodeParamsList Advanced) jobHandle = do
  markStarted 3 jobHandle
  corpusId <- view node_parent_id <$> getNode lId

  markProgress 1 jobHandle

  _ <- case corpusId of
    Just cId -> do
      _ <- Metrics.updatePie cId (Just lId) NgramsTypes.Authors Nothing
      _ <- Metrics.updateTree cId (Just lId) NgramsTypes.Institutes MapTerm
      _ <- Metrics.updatePie cId (Just lId) NgramsTypes.Sources Nothing
      pure ()
    Nothing  -> pure ()

  markComplete jobHandle

updateNode lId (UpdateNodeParamsList _mode) jobHandle = do
  markStarted 3 jobHandle
  corpusId <- view node_parent_id <$> getNode lId

  markProgress 1 jobHandle

  _ <- case corpusId of
    Just cId -> do
      _ <- reIndexWith cId lId NgramsTerms (Set.singleton MapTerm)
      _ <- updateNgramsOccurrences cId lId
      pure ()
    Nothing  -> pure ()

  markComplete jobHandle

updateNode phyloId (UpdateNodePhylo config) jobHandle = do
  markStarted 3 jobHandle
  corpusId' <- view node_parent_id <$> getNode phyloId
  markProgress 1 jobHandle

  let corpusId = fromMaybe (panicTrace "no corpus id") corpusId'

  phy <- timeMeasured "updateNode.flowPhyloAPI" $ flowPhyloAPI (subConfigAPI2config config) corpusId
  markProgress 2 jobHandle

{-
  logStatus JobLog { _scst_succeeded = Just 2
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }
-}
  _ <- timeMeasured "updateNode.updateHyperdataPhylo" $ updateHyperdata phyloId (HyperdataPhylo Nothing (Just phy))

  -- TODO: catch the error of sendMail if userId is not found, then debug
  -- sendMail (UserDBId userId)
  markComplete jobHandle

updateNode tId (UpdateNodeParamsTexts _mode) jobHandle = do
  markStarted 3 jobHandle
  corpusId <- view node_parent_id <$> getNode tId
  markProgress 1 jobHandle

  _ <- case corpusId of
    Just cId -> updateDocs cId
    Nothing  -> do
      _ <- panicTrace "[G.A.N.Update] updateNode/UpdateNodeParamsText: no corpus Id given"
      pure ()

  markComplete jobHandle


updateNode _nId _p jobHandle = do
  simuLogs jobHandle 10
------------------------------------------------------------------------

updateDocs :: (HasNodeStory env err m)
            => NodeId -> m ()
updateDocs cId = do
  lId <- defaultList cId
  _ <- reIndexWith cId lId NgramsTerms (Set.singleton MapTerm)
  _ <- updateNgramsOccurrences cId lId
  _ <- updateContextScore      cId lId
  _ <- Metrics.updateChart'    cId lId NgramsTypes.Docs Nothing
  -- printDebug "updateContextsScore" (cId, lId, u)
  pure ()

------------------------------------------------------------------------
-- TODO unPrefix "pn_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  UpdateNodeParams where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = GUA.defaultTaggedObject })

instance ToJSON    UpdateNodeParams where
  toJSON = genericToJSON (defaultOptions { sumEncoding = GUA.defaultTaggedObject })

instance ToSchema  UpdateNodeParams
instance Arbitrary UpdateNodeParams where
  arbitrary = do
    l <- UpdateNodeParamsList  <$> arbitrary
    g <- UpdateNodeParamsGraph <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    t <- UpdateNodeParamsTexts <$> arbitrary
    b <- UpdateNodeParamsBoard <$> arbitrary
    elements [l,g,t,b]

instance FromJSON  Method
instance ToJSON    Method
instance ToSchema  Method
instance Arbitrary Method where
  arbitrary = elements [ minBound .. maxBound ]

instance FromJSON  Granularity
instance ToJSON    Granularity
instance ToSchema  Granularity
instance Arbitrary Granularity where
  arbitrary = elements [ minBound .. maxBound ]

instance FromJSON  Charts
instance ToJSON    Charts
instance ToSchema  Charts
instance Arbitrary Charts where
  arbitrary = elements [ minBound .. maxBound ]

------------------------------------------------------------------------
