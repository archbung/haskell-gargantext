{-|
Module      : Gargantext.API.GraphQL.TreeFirstLevel
Description : 
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.TreeFirstLevel where

import Data.Morpheus.Types (GQLType)
import Gargantext.API.Admin.Auth.Types
import Gargantext.API.Auth.PolicyCheck
import Gargantext.API.GraphQL.PolicyCheck
import Gargantext.API.GraphQL.Types
import Gargantext.Core (fromDBid)
import Gargantext.Core.Types (Tree, NodeTree, NodeType)
import Gargantext.Core.Types.Main ( Tree(TreeN), _tn_node, _tn_children, NodeTree(NodeTree, _nt_id, _nt_type), _nt_name )
import Gargantext.Database.Admin.Config ()
import Gargantext.Database.Admin.Types.Node (allNodeTypes, NodeId (UnsafeMkNodeId))
import Gargantext.Database.Admin.Types.Node qualified as NN
import Gargantext.Database.Prelude (CmdCommon)
import Gargantext.Database.Query.Table.Node (getNode)
import Gargantext.Database.Query.Tree qualified as T
import Gargantext.Database.Schema.Node (NodePoly(_node_parent_id))
import Gargantext.Database.Schema.Node qualified as N
import Gargantext.Prelude

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

type ParentId = Maybe NodeId

resolveTree :: (CmdCommon env)
            => AuthenticatedUser
            -> AccessPolicyManager
            -> TreeArgs
            -> GqlM e env (TreeFirstLevel (GqlM e env))
resolveTree autUser mgr TreeArgs { root_id } =
  withPolicy autUser mgr (nodeChecks $ UnsafeMkNodeId root_id) $ dbTree root_id

dbTree :: (CmdCommon env) =>
          Int -> GqlM e env (TreeFirstLevel (GqlM e env))
dbTree root_id = do
  let rId = UnsafeMkNodeId root_id
  t <- lift $ T.tree T.TreeFirstLevel rId allNodeTypes
  n <- lift $ getNode $ UnsafeMkNodeId root_id
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
toTreeNode pId NodeTree { _nt_name, _nt_id, _nt_type } = TreeNode { name = _nt_name, id = NN._NodeId _nt_id, node_type = _nt_type, parent_id = NN._NodeId <$> pId}

childrenToTreeNodes :: (Tree NodeTree, NodeId) -> TreeNode
childrenToTreeNodes (TreeN {_tn_node}, rId) = toTreeNode (Just rId) _tn_node

resolveParent :: (CmdCommon env) => Maybe NodeId -> GqlM e env (Maybe TreeNode)
resolveParent (Just pId) = do
  node <- lift $ getNode pId
  pure $ nodeToTreeNode node
resolveParent Nothing = pure Nothing


nodeToTreeNode :: HasCallStack => NN.Node json -> Maybe TreeNode
nodeToTreeNode N.Node {..} = if (fromDBid _node_typename /= NN.NodeFolderShared) && (fromDBid _node_typename /= NN.NodeTeam)
                             then
                             Just TreeNode { id        = NN.unNodeId _node_id
                                           , name      = _node_name
                                           , node_type = fromDBid _node_typename
                                           , parent_id = NN.unNodeId <$> _node_parent_id
                                           }
                             else
                             Nothing

resolveBreadcrumb :: (CmdCommon env) => BreadcrumbArgs -> GqlM e env (BreadcrumbInfo)
resolveBreadcrumb BreadcrumbArgs { node_id } = dbRecursiveParents node_id

convertDbTreeToTreeNode :: HasCallStack => T.DbTreeNode -> TreeNode
convertDbTreeToTreeNode T.DbTreeNode { _dt_name, _dt_nodeId, _dt_typeId, _dt_parentId } = TreeNode
  { name = _dt_name
  , id = NN.unNodeId _dt_nodeId
  , node_type = fromDBid _dt_typeId
  , parent_id = NN.unNodeId <$> _dt_parentId
  }

dbRecursiveParents :: (CmdCommon env) => Int -> GqlM e env (BreadcrumbInfo)
dbRecursiveParents node_id = do
  let nId = UnsafeMkNodeId node_id
  dbParents <- lift $ T.recursiveParents nId allNodeTypes
  let treeNodes = map convertDbTreeToTreeNode dbParents
  let breadcrumbInfo = BreadcrumbInfo { parents = treeNodes }
  pure breadcrumbInfo  
