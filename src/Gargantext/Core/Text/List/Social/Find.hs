{-|
Module      : Gargantext.Core.Text.List.Social.Find
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Core.Text.List.Social.Find
  where

-- findList imports
import Control.Lens (view)

import Gargantext.Core (toDBid)
import Gargantext.Core.Types.Individu
import Gargantext.Database.Admin.Config ()
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (DBCmd)
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Query.Tree
import Gargantext.Database.Query.Tree.Root (getRootId)
import Gargantext.Prelude

------------------------------------------------------------------------
findListsId :: (HasNodeError err, HasTreeError err)
            => User -> NodeMode -> DBCmd err [NodeId]
findListsId u mode = do
  rootId <- getRootId u
  ns <- map (view dt_nodeId) <$> filter ((== toDBid NodeList) . (view dt_typeId))
                             <$> findNodes' rootId mode
  pure ns



-- | TODO not clear enough:
-- | Shared is for Shared with me but I am not the owner of it
-- | Private is for all Lists I have created
findNodes' :: (HasTreeError err, HasNodeError err)
          => RootId
          -> NodeMode
          -> DBCmd err [DbTreeNode]
findNodes' r Private = do
  pv <- (findNodes r Private $ [NodeFolderPrivate]          <> commonNodes)
  sh <- (findNodes' r Shared)
  pure $ pv <> sh
findNodes' r Shared       = findNodes r Shared  $ [NodeFolderShared, NodeTeam] <> commonNodes
findNodes' r SharedDirect = findNodes r Shared  $ [NodeFolderShared, NodeTeam] <> commonNodes
findNodes' r Public       = findNodes r Public  $ [NodeFolderPublic ]          <> commonNodes
findNodes' r PublicDirect = findNodes r Public  $ [NodeFolderPublic ]          <> commonNodes

commonNodes:: [NodeType]
commonNodes = [NodeFolder, NodeCorpus, NodeList, NodeFolderShared, NodeTeam]


