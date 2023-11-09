{-|
Module      : Gargantext.API.Members
Description :
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.API.Members where

import Gargantext.API.Admin.EnvTypes (Env)
import Gargantext.API.Errors.Types
import Gargantext.API.Prelude
import Gargantext.Database.Action.Share (membersOf)
import Gargantext.Database.Admin.Types.Node (NodeType(NodeTeam))
import Gargantext.Database.Prelude (CmdCommon)
import Gargantext.Database.Query.Table.Node (getNodesIdWithType)
import Gargantext.Prelude
import Servant

type MembersAPI = Get '[JSON] [Text]

members :: ServerT MembersAPI (GargM Env BackendInternalError)
members = getMembers

getMembers :: (CmdCommon env) =>
              GargM env BackendInternalError [Text]
getMembers = do
  teamNodeIds <- getNodesIdWithType NodeTeam
  m <- concatMapM membersOf teamNodeIds
  pure $ map fst m
