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
import Gargantext.API.Prelude
import Gargantext.Core.Types (UserId)
import Gargantext.Database.Action.Share (membersOf)
import Gargantext.Database.Admin.Types.Node (NodeType(NodeTeam))
import Gargantext.Database.Prelude (CmdCommon)
import Gargantext.Database.Query.Table.Node (getNodesIdWithType)
import Gargantext.Prelude
import Servant

type MembersAPI = Get '[JSON] [Text]

members :: UserId -> ServerT MembersAPI (GargM Env GargError)
members _ = do
  getMembers

getMembers :: (CmdCommon env) =>
              GargM env GargError [Text]
getMembers = do
  teamNodeIds <- getNodesIdWithType NodeTeam
  m <- concatMapM membersOf teamNodeIds
  pure $ map fst m
