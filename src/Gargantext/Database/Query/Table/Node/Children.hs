{-|
Module      : Gargantext.Database.Query.Table.Node.Children
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# LANGUAGE Arrows                      #-}

module Gargantext.Database.Query.Table.Node.Children
  where

import Control.Arrow (returnA)
import Gargantext.Core ( HasDBid(toDBid) )
import Gargantext.Core.Types
import Gargantext.Core.Types.Query (Limit, Offset)
import Gargantext.Database.Admin.Types.Hyperdata.Contact ( HyperdataContact )
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument )
import Gargantext.Database.Prelude (DBCmd, JSONB, runCountOpaQuery, runOpaQuery)
import Gargantext.Database.Query.Filter ( limit', offset' )
import Gargantext.Database.Query.Table.NodeContext ( NodeContextPoly(NodeContext), queryNodeContextTable )
import Gargantext.Database.Schema.Context
import Gargantext.Database.Schema.Node ( NodeRead, NodePoly(Node, _node_id), queryNodeTable )
import Gargantext.Prelude
import Opaleye


-- TODO getAllTableDocuments
getAllDocuments :: HasDBid NodeType => ParentId -> DBCmd err (TableResult (Node HyperdataDocument))
getAllDocuments pId = getAllChildren pId (Proxy :: Proxy HyperdataDocument)
                                         (Just NodeDocument)

-- TODO getAllTableContacts
getAllContacts :: HasDBid NodeType => ParentId -> DBCmd err (TableResult (Node HyperdataContact))
getAllContacts pId = getAllChildren pId (Proxy :: Proxy HyperdataContact)
                                        (Just NodeContact)

getAllChildren :: (JSONB a, HasDBid NodeType)
               => ParentId
               -> proxy a
               -> Maybe NodeType
               -> DBCmd err (NodeTableResult a)
getAllChildren pId p maybeNodeType = getChildren pId p maybeNodeType Nothing Nothing


getChildren :: (JSONB a, HasDBid NodeType)
            => ParentId
            -> proxy a
            -> Maybe NodeType
            -> Maybe Offset
            -> Maybe Limit
            -> DBCmd err (NodeTableResult a)
getChildren pId p t@(Just NodeDocument) maybeOffset maybeLimit = getChildrenContext pId p t maybeOffset maybeLimit
getChildren pId p t@(Just NodeContact ) maybeOffset maybeLimit = getChildrenContext pId p t maybeOffset maybeLimit
getChildren a b c d e = getChildrenNode a b c d e


getChildrenNode :: (JSONB a, HasDBid NodeType)
            => ParentId
            -> proxy a
            -> Maybe NodeType
            -> Maybe Offset
            -> Maybe Limit
            -> DBCmd err (NodeTableResult a)
getChildrenNode pId _ maybeNodeType maybeOffset maybeLimit = do
  -- printDebug "getChildrenNode" (pId, maybeNodeType)
  let query = selectChildrenNode pId maybeNodeType
  docs <- runOpaQuery
        $ limit'  maybeLimit
        $ offset' maybeOffset
        $ orderBy (asc _node_id)
        $ query
  docCount <- runCountOpaQuery query
  pure $ TableResult { tr_docs = docs, tr_count = docCount }


selectChildrenNode :: HasDBid NodeType
                   => ParentId
                   -> Maybe NodeType
                   -> Select NodeRead
selectChildrenNode parentId maybeNodeType = proc () -> do
    row@(Node _ _ typeName _ parent_id _ _ _) <- queryNodeTable -< ()
    let nodeType = maybe 0 toDBid maybeNodeType
    restrict -< typeName  .== sqlInt4 nodeType
    restrict -< parent_id .== (pgNodeId parentId)
    returnA -< row


getChildrenContext :: (JSONB a, HasDBid NodeType)
                   => ParentId
                   -> proxy a
                   -> Maybe NodeType
                   -> Maybe Offset
                   -> Maybe Limit
                   -> DBCmd err (NodeTableResult a)
getChildrenContext pId _ maybeNodeType maybeOffset maybeLimit = do
  -- printDebug "getChildrenContext" (pId, maybeNodeType)
  let query = selectChildren' pId maybeNodeType

  docs <- runOpaQuery
        $ limit'  maybeLimit
        $ offset' maybeOffset
        $ orderBy (asc _context_id)
        $ query

  docCount <- runCountOpaQuery query
  pure $ TableResult { tr_docs = map context2node docs, tr_count = docCount }


selectChildren' :: HasDBid NodeType
               => ParentId
               -> Maybe NodeType
               -> Select ContextRead
selectChildren' parentId maybeNodeType = proc () -> do
    row@(Context cid _ typeName _ _ _ _ _) <- queryContextTable     -< ()
    (NodeContext _ nid cid' _ _)           <- queryNodeContextTable -< ()

    let nodeType = maybe 0 toDBid maybeNodeType
    restrict -< typeName  .== sqlInt4 nodeType

    restrict -< nid .== pgNodeId parentId
    restrict -< cid .== cid'
    returnA -< row
