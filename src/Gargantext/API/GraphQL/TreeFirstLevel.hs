{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.TreeFirstLevel where

import Data.Morpheus.Types (GQLType, lift)
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Admin.Auth.Types
import Gargantext.API.Auth.PolicyCheck
import Gargantext.API.GraphQL.PolicyCheck
import Gargantext.API.GraphQL.Types
import Gargantext.Core.Types (Tree, NodeTree, NodeType)
import Gargantext.Core.Types.Main ( Tree(TreeN), _tn_node, _tn_children, NodeTree(NodeTree, _nt_id, _nt_type), _nt_name )
import Gargantext.Database.Admin.Config (fromNodeTypeId)
import Gargantext.Database.Admin.Types.Node (allNodeTypes, NodeId (NodeId))
import Gargantext.Database.Prelude (CmdCommon)
import Gargantext.Database.Query.Table.Node (getNode)
import Gargantext.Database.Schema.Node (NodePoly(_node_parent_id))
import Gargantext.Prelude
import qualified Gargantext.Database.Admin.Types.Node as NN
import qualified Gargantext.Database.Query.Tree as T
import qualified Gargantext.Database.Schema.Node as N

data TreeArgs = TreeArgs
  {
    root_id :: Int
  } deriving (Generic, GQLType)

data TreeNode = TreeNode
  {
    name      :: Text
  , id        :: Int
  , node_type :: NodeType
  , parent_id :: Maybe Int
  } deriving (Generic, GQLType)

data TreeFirstLevel m = TreeFirstLevel
  {
    root     :: TreeNode
  , parent   :: m (Maybe TreeNode)
  , children :: [TreeNode]
  } deriving (Generic, GQLType)

data BreadcrumbArgs = BreadcrumbArgs
  {
    node_id :: Int
  } deriving (Generic, GQLType)

data BreadcrumbInfo = BreadcrumbInfo
  {
    parents :: [TreeNode]
  } deriving (Generic, GQLType)


type GqlM e env = Resolver QUERY e (GargM env GargError)

type ParentId = Maybe NodeId

resolveTree :: (CmdCommon env)
            => AuthenticatedUser
            -> AccessPolicyManager
            -> TreeArgs
            -> GqlM e env (TreeFirstLevel (GqlM e env))
resolveTree autUser mgr TreeArgs { root_id } =
  withPolicy autUser mgr (nodeChecks (NodeId root_id)) $ dbTree root_id

dbTree :: (CmdCommon env) =>
          Int -> GqlM e env (TreeFirstLevel (GqlM e env))
dbTree root_id = do
  let rId = NodeId root_id
  t <- lift $ T.tree T.TreeFirstLevel rId allNodeTypes
  n <- lift $ getNode $ NodeId root_id
  let pId = toParentId n
  pure $ toTree rId pId t
  where
    toParentId N.Node { _node_parent_id } = _node_parent_id


toTree :: (CmdCommon env) => NodeId -> ParentId -> Tree NodeTree -> TreeFirstLevel (GqlM e env)
toTree rId pId TreeN { _tn_node, _tn_children } = TreeFirstLevel
  { parent   = resolveParent pId
  , root     = toTreeNode pId _tn_node
  , children = map childrenToTreeNodes $ zip _tn_children $ repeat rId
  }

toTreeNode :: ParentId -> NodeTree -> TreeNode
toTreeNode pId NodeTree { _nt_name, _nt_id, _nt_type } = TreeNode { name = _nt_name, id = id2int _nt_id, node_type = _nt_type, parent_id = id2int <$> pId}
  where
    id2int :: NodeId -> Int
    id2int (NodeId n) = n

childrenToTreeNodes :: (Tree NodeTree, NodeId) -> TreeNode
childrenToTreeNodes (TreeN {_tn_node}, rId) = toTreeNode (Just rId) _tn_node

resolveParent :: (CmdCommon env) => Maybe NodeId -> GqlM e env (Maybe TreeNode)
resolveParent (Just pId) = do
  node <- lift $ getNode pId
  pure $ nodeToTreeNode node
resolveParent Nothing = pure Nothing


nodeToTreeNode :: NN.Node json -> Maybe TreeNode
nodeToTreeNode N.Node {..} = if (fromNodeTypeId _node_typename /= NN.NodeFolderShared) && (fromNodeTypeId _node_typename /= NN.NodeTeam)
                             then
                             Just TreeNode { id        = NN.unNodeId _node_id
                                           , name      = _node_name
                                           , node_type = fromNodeTypeId _node_typename
                                           , parent_id = NN.unNodeId <$> _node_parent_id
                                           }
                             else
                             Nothing

resolveBreadcrumb :: (CmdCommon env) => BreadcrumbArgs -> GqlM e env (BreadcrumbInfo)
resolveBreadcrumb BreadcrumbArgs { node_id } = dbRecursiveParents node_id

convertDbTreeToTreeNode :: T.DbTreeNode -> TreeNode
convertDbTreeToTreeNode T.DbTreeNode { _dt_name, _dt_nodeId, _dt_typeId, _dt_parentId } = TreeNode
  { name = _dt_name
  , id = NN.unNodeId _dt_nodeId
  , node_type = fromNodeTypeId _dt_typeId
  , parent_id = NN.unNodeId <$> _dt_parentId
  }

dbRecursiveParents :: (CmdCommon env) => Int -> GqlM e env (BreadcrumbInfo)
dbRecursiveParents node_id = do
  let nId = NodeId node_id
  dbParents <- lift $ T.recursiveParents nId allNodeTypes
  let treeNodes = map convertDbTreeToTreeNode dbParents
  let breadcrumbInfo = BreadcrumbInfo { parents = treeNodes }
  pure breadcrumbInfo  
