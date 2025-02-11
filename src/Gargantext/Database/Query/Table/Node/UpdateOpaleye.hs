{-|
Module      : Gargantext.Database.Node.UpdateOpaleye
Description : Update Node in Database (Postgres)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Database.Query.Table.Node.UpdateOpaleye
  where

import Data.Aeson (encode)
import Gargantext.Core ( HasDBid )
import Gargantext.Database.Admin.Types.Hyperdata.Prelude ( HyperdataC )
import Gargantext.Database.Admin.Types.Node (NodeType, pgNodeId, NodeId)
import Gargantext.Database.Prelude (mkCmd, DBCmd)
import Gargantext.Database.Query.Table.Node ( getNodeWithType, getNodesIdWithType, getNodesWithType )
import Gargantext.Database.Query.Table.Node.Error ( HasNodeError )
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import Opaleye

-- import Debug.Trace (trace)

updateHyperdata :: HyperdataC a => NodeId -> a -> DBCmd err Int64
updateHyperdata i h = mkCmd $ \c -> putStrLn ("before runUpdate_" :: Text) >>
                                    runUpdate_ c (updateHyperdataQuery i h) >>= \res ->
                                    putStrLn ("after runUpdate_" :: Text) >> pure res

updateHyperdataQuery :: HyperdataC a => NodeId -> a -> Update Int64
updateHyperdataQuery i h = seq h' $ {- trace "updateHyperdataQuery: encoded JSON" $ -} Update
   { uTable      = nodeTable
   , uUpdateWith = updateEasy (\  (Node { .. })
                                -> Node { _node_hyperdata = h', .. }
                               -- -> trace "updating mate" $ Node _ni _nh _nt _nu _np _nn _nd h'
                              )
   , uWhere      = \row -> {-trace "uWhere" $-} _node_id row .== pgNodeId i
   , uReturning  = rCount
   }
    where h' =  sqlJSONB $ cs $ encode h

----------------------------------------------------------------------------------
updateNodesWithType :: ( HasNodeError err
                       , HasDBid NodeType
                       , HyperdataC a
                       ) => NodeType -> proxy a -> (a -> a) -> DBCmd err [Int64]
updateNodesWithType nt p f = do
  ns <- getNodesWithType nt p
  mapM (\n -> updateHyperdata (_node_id n) (f $ _node_hyperdata n)) ns

updateNodeWithType :: ( HasNodeError err
                      , HasDBid NodeType
                      , HyperdataC a
                      ) => NodeId
                        -> NodeType
                        -> proxy a
                        -> (a -> a)
                        -> DBCmd err [Int64]
updateNodeWithType nId nt p f = do
  ns <- getNodeWithType nId nt p
  mapM (\n -> updateHyperdata (_node_id n) (f $ _node_hyperdata n)) ns


-- | In case the Hyperdata Types are not compatible
updateNodesWithType_ :: ( HasNodeError err
                        , HyperdataC a
                        , HasDBid NodeType
                        ) => NodeType -> a -> DBCmd err [Int64]
updateNodesWithType_ nt h = do
  ns <- getNodesIdWithType nt
  mapM (\n -> updateHyperdata n h) ns
