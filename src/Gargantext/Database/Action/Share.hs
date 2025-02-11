{-|
Module      : Gargantext.Database.Action.Share
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


{-# LANGUAGE Arrows       #-}
{-# LANGUAGE ViewPatterns #-}

module Gargantext.Database.Action.Share
  where

import Control.Arrow (returnA)
import Control.Lens (view, (^.))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database
import Gargantext.Database.Action.User (getUserId)
import Gargantext.Database.Admin.Config (hasNodeType, isInNodeTypes)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataAny(..))
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node (getNode, getNodesWith)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError, errorWith)
import Gargantext.Database.Query.Table.NodeNode (deleteNodeNode, queryNodeNodeTable)
import Gargantext.Database.Query.Table.User
import Gargantext.Database.Query.Tree.Root (getRootId)
import Gargantext.Database.Prelude (Cmd, runOpaQuery)
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import Gargantext.Utils.Tuple (uncurryMaybe)
import Opaleye hiding (not)
import Opaleye qualified as O

-- | TODO move in PhyloConfig of Gargantext
publicNodeTypes :: [NodeType]
publicNodeTypes = [NodeDashboard, NodeGraph, NodePhylo, NodeFile]

------------------------------------------------------------------------
data ShareNodeWith = ShareNodeWith_User { snwu_nodetype :: NodeType
                                        , snwu_user     :: User
                                        }
                   | ShareNodeWith_Node { snwn_nodetype :: NodeType
                                        , snwn_node_id  :: NodeId
                                        }
------------------------------------------------------------------------
deleteMemberShip :: HasNodeError err => [(SharedFolderId, TeamNodeId)] -> Cmd err [Int]
deleteMemberShip xs = mapM (\(s,t) -> deleteNodeNode s t) xs

------------------------------------------------------------------------

type SharedFolderId = NodeId
type TeamNodeId     = NodeId

-- List members of a Team
-- Result gives the username and its SharedFolderId that has to be eventually
-- used for the membership
membersOf :: HasNodeError err
          => TeamNodeId -> Cmd err [(Text, SharedFolderId)]
membersOf nId = do
  res <- runOpaQuery $ membersOfQuery nId
  pure $ catMaybes (uncurryMaybe <$> res)


membersOfQuery :: TeamNodeId
               -> SelectArr () (MaybeFields (Field SqlText), MaybeFields (Field SqlInt4))
membersOfQuery (_NodeId -> teamId) = proc () -> do
  (nn, n, u) <- nodeNode_node_User -< ()
  restrict -< (nn ^. nn_node2_id) .== sqlInt4 teamId
  returnA -< ( user_username <$> u
             , view node_id <$> n )


nodeNode_node_User :: O.Select ( NodeNodeRead
                               , MaybeFields NodeRead
                               , MaybeFields UserRead )
nodeNode_node_User = proc () -> do
  nn <- queryNodeNodeTable -< ()
  n <- optionalRestrict queryNodeTable -<
    \n' -> (n' ^. node_id) .== (nn ^. nn_node1_id)
  u <- optionalRestrict queryUserTable -<
    \u' -> (view node_user_id <$> n) .=== justFields (user_id u')

  returnA -< (nn, n, u)

------------------------------------------------------------------------
-- To Share a Node Team with a user, use this function
-- basically used with the invitation to a team
shareNodeWith :: HasNodeError err
              => ShareNodeWith
              -> NodeId
              -> Cmd err Int
shareNodeWith (ShareNodeWith_User NodeFolderShared u) n = do
  nodeToCheck <- getNode   n
  userIdCheck <- getUserId u
  if not (hasNodeType nodeToCheck NodeTeam)
    then errorWith "[G.D.A.S.shareNodeWith] Can share node Team only"
    else
      if (view node_user_id nodeToCheck == userIdCheck)
        then errorWith "[G.D.A.S.shareNodeWith] Can share to others only"
        else do
          folderSharedId  <- getFolderId u NodeFolderShared
          insertDB ([NodeNode { _nn_node1_id = folderSharedId
                              , _nn_node2_id = n
                              , _nn_score = Nothing
                              , _nn_category = Nothing }]:: [NodeNode])

shareNodeWith (ShareNodeWith_Node NodeFolderPublic nId) n = do
  nodeToCheck <- getNode n
  if not (isInNodeTypes nodeToCheck publicNodeTypes)
    then errorWith $ "[G.D.A.S.shareNodeWith] Can share this nodesTypes only: "
                   <> (show publicNodeTypes)
    else do
      folderToCheck <- getNode nId
      if hasNodeType folderToCheck NodeFolderPublic
         then insertDB ([NodeNode { _nn_node1_id = nId
                                  , _nn_node2_id = n
                                  , _nn_score = Nothing
                                  , _nn_category = Nothing }] :: [NodeNode])
         else errorWith "[G.D.A.S.shareNodeWith] Can share NodeWith NodeFolderPublic only"

shareNodeWith _ _ = errorWith "[G.D.A.S.shareNodeWith] Not implemented for this NodeType"

------------------------------------------------------------------------
getFolderId :: HasNodeError err => User -> NodeType -> Cmd err NodeId
getFolderId u nt = do
  rootId <- getRootId u
  s <- getNodesWith rootId HyperdataAny (Just nt) Nothing Nothing
  case head s of
    Nothing -> errorWith "[G.D.A.S.getFolderId] No folder shared found"
    Just  f -> pure (_node_id f)

------------------------------------------------------------------------
type TeamId = NodeId

delFolderTeam :: HasNodeError err => User -> TeamId -> Cmd err Int
delFolderTeam u nId = do
  folderSharedId <- getFolderId u NodeFolderShared
  deleteNodeNode folderSharedId nId

unPublish :: HasNodeError err
          => ParentId -> NodeId
          -> Cmd err Int
unPublish p n = deleteNodeNode p n
