{-|
Module      : Gargantext.API.Node
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-- TODO-SECURITY: Critical

-- TODO-ACCESS: CanGetNode
-- TODO-EVENTS: No events as this is a read only query.
Node API
-------------------------------------------------------------------
-- TODO-ACCESS: access by admin only.
--              At first let's just have an isAdmin check.
--              Later: check userId CanDeleteNodes Nothing
-- TODO-EVENTS: DeletedNodes [NodeId]
--              {"tag": "DeletedNodes", "nodes": [Int*]}

-}

{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}

module Gargantext.API.Node
  where

import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Swagger
import Gargantext.API.Admin.Auth (withAccess, withPolicy)
import Gargantext.API.Admin.Auth.Types (PathId(..), AuthenticatedUser (..), auth_node_id)
import Gargantext.API.Admin.EnvTypes
import Gargantext.API.Auth.PolicyCheck
import Gargantext.API.Metrics
import Gargantext.API.Ngrams (TableNgramsApi, apiNgramsTableCorpus)
import Gargantext.API.Ngrams.Types (TabType(..))
import Gargantext.API.Node.DocumentUpload qualified as DocumentUpload
import Gargantext.API.Node.File
import Gargantext.API.Node.FrameCalcUpload qualified as FrameCalcUpload
import Gargantext.API.Node.New
import Gargantext.API.Node.Share qualified as Share
import Gargantext.API.Node.Update qualified as Update
import Gargantext.API.Prelude
import Gargantext.API.Search qualified as Search
import Gargantext.API.Table
import Gargantext.Core.Types (NodeTableResult)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Types.Main (Tree, NodeTree)
import Gargantext.Core.Types.Query (Limit, Offset)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Core.Viz.Phylo.API (PhyloAPI, phyloAPI)
import Gargantext.Database.Action.Delete qualified as Action (deleteNode)
import Gargantext.Database.Action.Flow.Pairing (pairing)
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd, JSONB)
import Gargantext.Database.Query.Facet (FacetDoc, OrderBy(..))
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Children (getChildren)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Query.Table.Node.Update (Update(..), update)
import Gargantext.Database.Query.Table.Node.Update qualified as U (update, Update(..))
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Query.Table.NodeContext (nodeContextsCategory, nodeContextsScore)
import Gargantext.Database.Query.Table.NodeNode
import Gargantext.Database.Query.Tree (tree, tree_flat, TreeMode(..))
import Gargantext.Prelude
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Gargantext.API.Node.DocumentsFromWriteNodes as DFWN


-- | Admin NodesAPI
-- TODO
type NodesAPI  = Delete '[JSON] Int

-- | Delete Nodes
-- Be careful: really delete nodes
-- Access by admin only
nodesAPI :: [NodeId] -> GargServer NodesAPI
nodesAPI = deleteNodes

------------------------------------------------------------------------
-- | TODO-ACCESS: access by admin only.
-- At first let's just have an isAdmin check.
-- Later: CanAccessAnyNode or (CanGetAnyNode, CanPutAnyNode)
-- To manage the Users roots
-- TODO-EVENTS:
--   PutNode ?
-- TODO needs design discussion.
type Roots =  Get    '[JSON] [Node HyperdataUser]
         :<|> Put    '[JSON] Int -- TODO

-- | TODO: access by admin only
roots :: GargServer Roots
roots = getNodesWithParentId Nothing
    :<|> pure (panic "not implemented yet") -- TODO use patch map to update what we need

-------------------------------------------------------------------
-- | Node API Types management
-- TODO-ACCESS : access by users
-- No ownership check is needed if we strictly follow the capability model.
--
-- CanGetNode (Node, Children, TableApi, TableNgramsApiGet, PairingApi, ChartApi,
--             SearchAPI)
-- CanRenameNode (or part of CanEditNode?)
-- CanCreateChildren (PostNodeApi)
-- CanEditNode / CanPutNode TODO not implemented yet
-- CanDeleteNode
-- CanPatch (TableNgramsApi)
-- CanFavorite
-- CanMoveToTrash

type NodeAPI a = PolicyChecked (NodeNodeAPI a)
             :<|> "rename" :> RenameApi
             :<|> PostNodeApi -- TODO move to children POST
             :<|> PostNodeAsync
             :<|> FrameCalcUpload.API
             :<|> ReqBody '[JSON] a :> Put    '[JSON] Int
             :<|> "update"     :> Update.API
             :<|> Delete '[JSON] Int
             :<|> "children"  :> ChildrenApi a

             -- TODO gather it
             :<|> "table"      :> TableApi
             :<|> "ngrams"     :> TableNgramsApi

             :<|> "category"   :> CatApi
             :<|> "score"      :> ScoreApi
             :<|> "search"     :> (Search.API Search.SearchResult)
             :<|> "share"      :> Share.API

             -- Pairing utilities
             :<|> "pairwith"   :> PairWith
             :<|> "pairs"      :> Pairs
             :<|> "pairing"    :> PairingApi

             -- VIZ
             :<|> "metrics"   :> ScatterAPI
             :<|> "chart"     :> ChartApi
             :<|> "pie"       :> PieApi
             :<|> "tree"      :> TreeApi
             :<|> "phylo"     :> PhyloAPI
             -- :<|> "add"       :> NodeAddAPI
             :<|> "move"      :> MoveAPI
             :<|> "unpublish" :> Share.Unpublish

             :<|> "file"      :> FileApi
             :<|> "async"     :> FileAsyncApi

             :<|> "documents-from-write-nodes" :> DFWN.API
             :<|> DocumentUpload.API

-- TODO-ACCESS: check userId CanRenameNode nodeId
-- TODO-EVENTS: NodeRenamed RenameNode or re-use some more general NodeEdited...
type RenameApi = Summary " Rename Node"
               :> ReqBody '[JSON] RenameNode
               :> Put     '[JSON] [Int]

type PostNodeApi = Summary " PostNode Node with ParentId as {id}"
                 :> ReqBody '[JSON] PostNode
                 :> Post    '[JSON] [NodeId]

type ChildrenApi a = Summary " Summary children"
                 :> QueryParam "type"   NodeType
                 :> QueryParam "offset" Offset
                 :> QueryParam "limit"  Limit
                 -- :> Get '[JSON] [Node a]
                 :> Get '[JSON] (NodeTableResult a)

------------------------------------------------------------------------
type NodeNodeAPI a = Get '[JSON] (Node a)

nodeNodeAPI :: forall proxy a. (JSONB a, ToJSON a)
            => proxy a
            -> AuthenticatedUser
            -> CorpusId
            -> NodeId
            -> GargServer (NodeNodeAPI a)
nodeNodeAPI p uId cId nId = withAccess (Proxy :: Proxy (NodeNodeAPI a)) Proxy uId (PathNodeNode cId nId) nodeNodeAPI'
  where
    nodeNodeAPI' :: GargServer (NodeNodeAPI a)
    nodeNodeAPI' = getNodeWith nId p

------------------------------------------------------------------------
-- TODO: make the NodeId type indexed by `a`, then we no longer need the proxy.
nodeAPI :: forall proxy a.
       ( HyperdataC a, Show a
       ) => proxy a
         -> AuthenticatedUser
         -> NodeId
         -> ServerT (NodeAPI a) (GargM Env GargError)
nodeAPI p authenticatedUser targetNode =
  withAccess (Proxy :: Proxy (NodeAPI a)) Proxy authenticatedUser (PathNode targetNode) nodeAPI'
  where

    userRootId = RootId $ authenticatedUser ^. auth_node_id

    nodeAPI' :: ServerT (NodeAPI a) (GargM Env GargError)
    nodeAPI' =  withPolicy authenticatedUser (nodeChecks targetNode) (getNodeWith targetNode p)
           :<|> rename                                targetNode
           :<|> postNode            authenticatedUser targetNode
           :<|> postNodeAsyncAPI    authenticatedUser targetNode
           :<|> FrameCalcUpload.api authenticatedUser targetNode
           :<|> putNode                               targetNode
           :<|> Update.api                            targetNode
           :<|> Action.deleteNode   userRootId        targetNode
           :<|> getChildren                           targetNode p

           -- TODO gather it
           :<|> tableApi                              targetNode
           :<|> apiNgramsTableCorpus                  targetNode

           :<|> catApi                                targetNode
           :<|> scoreApi                              targetNode
           :<|> Search.api                            targetNode
           :<|> Share.api           userRootId        targetNode
           -- Pairing Tools
           :<|> pairWith                              targetNode
           :<|> pairs                                 targetNode
           :<|> getPair                               targetNode

           -- VIZ
           :<|> scatterApi                            targetNode
           :<|> chartApi                              targetNode
           :<|> pieApi                                targetNode
           :<|> treeApi                               targetNode
           :<|> phyloAPI                              targetNode
           :<|> moveNode           userRootId         targetNode
           -- :<|> nodeAddAPI id'
           -- :<|> postUpload id'
           :<|> Share.unPublish                       targetNode

           :<|> fileApi                               targetNode
           :<|> fileAsyncApi       authenticatedUser  targetNode

           :<|> DFWN.api           authenticatedUser  targetNode
           :<|> DocumentUpload.api                    targetNode


------------------------------------------------------------------------
data RenameNode = RenameNode { r_name :: Text }
  deriving (Generic)

------------------------------------------------------------------------
------------------------------------------------------------------------
type CatApi =  Summary " To Categorize NodeNodes: 0 for delete, 1/null neutral, 2 favorite"
            :> ReqBody '[JSON] NodesToCategory
            :> Put     '[JSON] [Int]

data NodesToCategory = NodesToCategory { ntc_nodesId :: [NodeId]
                                       , ntc_category :: Int
                                       }
  deriving (Generic)

-- TODO unPrefix "ntc_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  NodesToCategory
instance ToJSON    NodesToCategory
instance ToSchema  NodesToCategory

catApi :: CorpusId -> GargServer CatApi
catApi cId cs' = do
  ret <- nodeContextsCategory $ map (\n -> (cId, n, ntc_category cs')) (ntc_nodesId cs')
  lId <- defaultList cId
  _ <- updateChart cId (Just lId) Docs Nothing
  pure ret

------------------------------------------------------------------------
type ScoreApi =  Summary " To Score NodeNodes"
            :> ReqBody '[JSON] NodesToScore
            :> Put     '[JSON] [Int]

data NodesToScore = NodesToScore { nts_nodesId :: [NodeId]
                                 , nts_score :: Int
                                 }
  deriving (Generic)

-- TODO unPrefix "ntc_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  NodesToScore
instance ToJSON    NodesToScore
instance ToSchema  NodesToScore

scoreApi :: CorpusId -> GargServer ScoreApi
scoreApi = putScore
  where
    putScore :: CorpusId -> NodesToScore -> Cmd err [Int]
    putScore cId cs' = nodeContextsScore $ map (\n -> (cId, n, nts_score cs')) (nts_nodesId cs')

------------------------------------------------------------------------
-- TODO adapt FacetDoc -> ListDoc (and add type of document as column)
-- Pairing utilities to move elsewhere
type PairingApi = Summary " Pairing API"
                :> QueryParam "view"   TabType
                -- TODO change TabType -> DocType (CorpusId for pairing)
                :> QueryParam "offset" Offset
                :> QueryParam "limit"  Limit
                :> QueryParam "order"  OrderBy
                :> Get '[JSON] [FacetDoc]

----------
type Pairs    = Summary "List of Pairs"
              :> Get '[JSON] [AnnuaireId]
pairs :: CorpusId -> GargServer Pairs
pairs cId = do
  ns <- getNodeNode cId
  pure $ map _nn_node2_id ns

type PairWith = Summary "Pair a Corpus with an Annuaire"
              :> "annuaire" :> Capture "annuaire_id" AnnuaireId
              :> QueryParam "list_id"     ListId
              :> Post '[JSON] [Int]

pairWith :: CorpusId -> GargServer PairWith
pairWith cId aId lId = do
  r <- pairing cId aId lId
  _ <- insertNodeNode [ NodeNode { _nn_node1_id = cId
                                 , _nn_node2_id = aId
                                 , _nn_score = Nothing
                                 , _nn_category = Nothing }]
  pure r


------------------------------------------------------------------------
type TreeAPI   = QueryParams "type" NodeType
                  :> Get '[JSON] (Tree NodeTree)
                  :<|> "first-level"
                      :> QueryParams "type" NodeType
                      :> Get '[JSON] (Tree NodeTree)

treeAPI :: NodeId -> GargServer TreeAPI
treeAPI id = tree TreeAdvanced id
        :<|> tree TreeFirstLevel id

type TreeFlatAPI = QueryParams "type" NodeType
                    :> QueryParam "query" Text
                    :> Get '[JSON] [NodeTree]

treeFlatAPI :: NodeId -> GargServer TreeFlatAPI
treeFlatAPI = tree_flat

------------------------------------------------------------------------
-- | TODO Check if the name is less than 255 char
rename :: NodeId -> RenameNode -> Cmd err [Int]
rename nId (RenameNode name') = U.update (U.Rename nId name')

putNode :: forall err a. (HasNodeError err, HyperdataC a)
        => NodeId
        -> a
        -> Cmd err Int
putNode n h = fromIntegral <$> updateHyperdata n h

-------------------------------------------------------------
type MoveAPI  = Summary "Move Node endpoint"
              :> Capture "parent_id" ParentId
              :> Put '[JSON] [Int]

moveNode :: User
         -> NodeId
         -> ParentId
         -> Cmd err [Int]
moveNode _u n p = update (Move n p)
-------------------------------------------------------------


$(deriveJSON (unPrefix "r_"       ) ''RenameNode )
instance ToSchema  RenameNode
instance Arbitrary RenameNode where
  arbitrary = elements [RenameNode "test"]


-------------------------------------------------------------
