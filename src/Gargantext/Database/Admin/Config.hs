{-|
Module      : Gargantext.Database
Description : Tools for Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Target: just import this module and nothing else to work with
Gargantext's database.

TODO: configure nodes table in Haskell (Config typenames etc.)
-}


{-# OPTIONS_GHC -fno-warn-orphans        #-}

module Gargantext.Database.Admin.Config
    where

import Control.Lens (view)
import Gargantext.Core (HasDBid(..))
import Data.Bimap qualified as Bimap
import Data.Bimap (Bimap)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Schema.Node
import Gargantext.Prelude

-- TODO put this in config.ini file
corpusMasterName :: Text
corpusMasterName = "Main"

userMaster :: Text
userMaster = "gargantua"

userArbitrary :: Text
userArbitrary = "user1"

instance HasDBid NodeType where
  toDBid   n   = nodeTypes Bimap.! n -- nodeTypes is total, this cannot fail by construction
  lookupDBid i = Bimap.lookupR i nodeTypes


hasNodeType :: forall a. Node a -> NodeType -> Bool
hasNodeType n nt = (view node_typename n) == (toDBid nt)

isInNodeTypes :: forall a. Node a -> [NodeType] -> Bool
isInNodeTypes n ts = elem (view node_typename n) (map toDBid ts)

-- | Nodes are typed in the database according to a specific ID
--
nodeTypes :: Bimap NodeType NodeTypeId
nodeTypes = Bimap.fromList $ allNodeTypes <&> \n -> case n of
    NodeUser          -> (n, 1)
    NodeFolder        -> (n, 2)
    NodeFolderPrivate -> (n, 20)
    NodeFolderShared  -> (n, 21)
    NodeTeam          -> (n, 210)
    NodeFolderPublic  -> (n, 22)
    NodeCorpusV3      -> (n, 3)
    NodeCorpus        -> (n, 30)
    NodeAnnuaire      -> (n, 31)
    NodeTexts         -> (n, 40)
    NodeDocument      -> (n, 4)
    NodeContact       -> (n, 41)
    NodeList          -> (n, 5)
    NodeListCooc      -> (n, 50)
    NodeModel         -> (n, 52)
    NodeGraph         -> (n, 9)
    NodePhylo         -> (n, 90)
    NodeDashboard     -> (n, 71)
    NodeFile          -> (n, 101)
    Notes             -> (n, 991)
    Calc              -> (n, 992)
    NodeFrameNotebook -> (n, 993)
    NodeFrameVisio    -> (n, 994)
