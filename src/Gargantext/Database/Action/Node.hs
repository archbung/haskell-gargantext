{-|
Module      : Gargantext.Database.Action.Node
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Gargantext.Database.Action.Node
  where

import Control.Lens (view)
import Gargantext.Core
import Gargantext.Core.Types (Name)
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Hyperdata.Default
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (DBCmd, HasConfig(..))
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Prelude hiding (hash)
import Gargantext.Prelude.Config (GargConfig(..))
import Gargantext.Prelude.Crypto.Hash (hash)

------------------------------------------------------------------------
-- | TODO mk all others nodes
mkNodeWithParent :: (HasNodeError err, HasDBid NodeType)
                 => NodeType
                 -> Maybe ParentId
                 -> UserId
                 -> Name
                 -> DBCmd err [NodeId]
mkNodeWithParent NodeUser (Just pId) uid  _ = nodeError $ NodeCreationFailed $ UserParentAlreadyExists uid pId

------------------------------------------------------------------------
-- | MkNode, insert and eventually configure Hyperdata
mkNodeWithParent NodeUser Nothing uId name =
  insertNodesWithParentR Nothing [node NodeUser name defaultHyperdataUser Nothing uId]

mkNodeWithParent _ Nothing uId _ = nodeError $ NodeCreationFailed $ UserParentDoesNotExist uId
------------------------------------------------------------------------
mkNodeWithParent Notes i u n =
  mkNodeWithParent_ConfigureHyperdata Notes i u n

mkNodeWithParent Calc i u n =
  mkNodeWithParent_ConfigureHyperdata Calc i u n

mkNodeWithParent NodeFrameVisio i u n =
  mkNodeWithParent_ConfigureHyperdata NodeFrameVisio i u n

mkNodeWithParent NodeFrameNotebook i u n =
  mkNodeWithParent_ConfigureHyperdata NodeFrameNotebook i u n



mkNodeWithParent nt (Just pId) uId name  = (:[]) <$> insertNode nt (Just name) Nothing pId uId
-- mkNodeWithParent _ _ _ _ = errorWith "[G.D.A.Node.mkNodeWithParent] nees parent"


-- | Sugar to create a node, get its NodeId and update its Hyperdata after
mkNodeWithParent_ConfigureHyperdata :: (HasNodeError err, HasDBid NodeType)
                                    => NodeType
                                    -> Maybe ParentId
                                    -> UserId
                                    -> Name
                                    -> DBCmd err [NodeId]
mkNodeWithParent_ConfigureHyperdata Notes (Just i) uId name =
  mkNodeWithParent_ConfigureHyperdata' Notes (Just i) uId name

mkNodeWithParent_ConfigureHyperdata Calc (Just i) uId name =
  mkNodeWithParent_ConfigureHyperdata' Calc (Just i) uId name

mkNodeWithParent_ConfigureHyperdata NodeFrameVisio (Just i) uId name =
  mkNodeWithParent_ConfigureHyperdata' NodeFrameVisio (Just i) uId name

mkNodeWithParent_ConfigureHyperdata NodeFrameNotebook (Just i) uId name = (:[]) <$>
  insertNode NodeFrameNotebook  (Just "Notebook")
                                (Just $ DefaultFrameCode $ HyperdataFrame { _hf_base = "Codebook"
                                                                          , _hf_frame_id = name }) i uId

mkNodeWithParent_ConfigureHyperdata    _ _ _ _ = nodeError NotImplYet


-- | Function not exposed
mkNodeWithParent_ConfigureHyperdata' :: (HasNodeError err, HasDBid NodeType)
                                    => NodeType
                                    -> Maybe ParentId
                                    -> UserId
                                    -> Name
                                    -> DBCmd err [NodeId]
mkNodeWithParent_ConfigureHyperdata' nt (Just i) uId name = do
  nodeId <- case nt of
     Notes -> insertNode Notes (Just name)   Nothing i uId
     Calc  -> insertNode Calc  (Just name)   Nothing i uId
     NodeFrameVisio -> insertNode NodeFrameVisio (Just name)   Nothing i uId
     _              -> nodeError NeedsConfiguration

  cfg <- view hasConfig
  u <- case nt of
        Notes -> pure $ _gc_frame_write_url cfg
        Calc  -> pure $ _gc_frame_calc_url  cfg
        NodeFrameVisio -> pure $ _gc_frame_visio_url  cfg
        _              -> nodeError NeedsConfiguration
  let
    s = _gc_secretkey cfg
    hd = HyperdataFrame u (hash $ s <> (show nodeId))
  _ <- updateHyperdata nodeId hd
  pure [nodeId]
mkNodeWithParent_ConfigureHyperdata' _ Nothing uId _ = nodeError $ NodeCreationFailed $ UserParentDoesNotExist uId
