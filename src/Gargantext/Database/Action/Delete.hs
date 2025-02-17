{-|
Module      : Gargantext.Database.Action.Delete
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TODO: right managements of nodes children of node Team
-- TODO add proper Right Management Type

TODO: NodeError

-}

module Gargantext.Database.Action.Delete
  where

import Control.Lens     (view, (^.))
import Data.Text
import Servant

import Gargantext.Core
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Share (delFolderTeam)
import Gargantext.Database.Action.User (getUserId)
import Gargantext.Database.Admin.Types.Hyperdata.File
import Gargantext.Database.Admin.Types.Node -- (NodeType(..))
import Gargantext.Database.Prelude (Cmd', CmdCommon)
import Gargantext.Database.Query.Table.Node (getNodeWith)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import qualified Gargantext.Database.GargDB as GargDB
import qualified Gargantext.Database.Query.Table.Node as N (getNode, deleteNode)

------------------------------------------------------------------------
-- TODO
-- Delete Corpus children accoring its types
-- Delete NodeList (NodeStory + cbor file)
deleteNode :: (CmdCommon env, HasNodeError err)
           => User
           -> NodeId
           -> Cmd' env err Int
deleteNode u nodeId = do
  node' <- N.getNode nodeId
  case (view node_typename node') of
    nt | nt == toDBid NodeUser -> panicTrace "[G.D.A.D.deleteNode] Not allowed to delete NodeUser (yet)"
    nt | nt == toDBid NodeTeam -> do
      uId   <- getUserId u
      if _node_user_id node' == uId
        then N.deleteNode    nodeId
        else delFolderTeam u nodeId
    nt | nt == toDBid NodeFile -> do
      node <- getNodeWith nodeId (Proxy :: Proxy HyperdataFile)
      let (HyperdataFile { _hff_path = path }) = node ^. node_hyperdata
      GargDB.rmFile $ unpack path
      N.deleteNode nodeId
    _                             -> N.deleteNode nodeId

  -- if hasNodeType node' NodeUser
  --    then panic "Not allowed to delete NodeUser (yet)"
  --    else if hasNodeType node' NodeTeam
  --            then do
  --               uId   <- getUserId u
  --               if _node_user_id node' == uId
  --                  then N.deleteNode    nodeId
  --                  else delFolderTeam u nodeId
  --            else N.deleteNode nodeId
