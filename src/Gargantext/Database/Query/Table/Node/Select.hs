{-|
Module      : Gargantext.Database.Node.Select
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE Arrows            #-}
{-# LANGUAGE LambdaCase        #-}

module Gargantext.Database.Query.Table.Node.Select
  where

import Control.Arrow (returnA)
import Gargantext.Core ( HasDBid(toDBid) )
import Gargantext.Database.Admin.Types.Node ( NodeType, NodeId )
import Gargantext.Core.Types.Individu (Username)
import Gargantext.Database.Prelude (DBCmd, runOpaQuery)
import Gargantext.Database.Query.Table.User ( UserPoly(user_username, user_id), queryUserTable )
import Gargantext.Database.Schema.Node ( NodePoly(_node_id, _node_user_id, _node_typename), queryNodeTable )
import Opaleye
import Protolude

selectNodesWithUsername :: (HasDBid NodeType) => NodeType -> Username -> DBCmd err [NodeId]
selectNodesWithUsername nt u = runOpaQuery $ proc () -> do
  n <- queryNodeTable -< ()
  usrs <- optionalRestrict queryUserTable -<
          (\us' -> _node_user_id n .== user_id us')
  restrict -< matchMaybe usrs $ \case
    Nothing -> toFields True
    Just us -> user_username us .== sqlStrictText u
  restrict -< _node_typename n .== sqlInt4 (toDBid nt)
  returnA  -< _node_id n
