{-|
Module      : Gargantext.Database.Query.Table.Node
Description : Main Tools of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeFamilies           #-}

module Gargantext.Database.Query.Table.Context
  where

import Control.Arrow (returnA)
import Gargantext.Core ( HasDBid(toDBid) )
import Gargantext.Core.Types
import Gargantext.Core.Types.Query (Limit, Offset)
import Gargantext.Database.Admin.Types.Hyperdata.Any ( HyperdataAny )
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument, HyperdataDocumentV3 )
import Gargantext.Database.Prelude (DBCmd, JSONB, runOpaQuery)
import Gargantext.Database.Query.Filter (limit', offset')
import Gargantext.Database.Query.Table.Node.Error ( HasNodeError, nodeError, NodeError(NoContextFound) )
import Gargantext.Database.Schema.Context
import Gargantext.Prelude hiding (sum, head)
import Opaleye            hiding (FromField)
import Prelude            hiding (null, id, map, sum)


getContextWith :: (HasNodeError err, JSONB a)
            => ContextId -> proxy a -> DBCmd err (Node a)
getContextWith cId _ = do
  maybeContext <- headMay <$> runOpaQuery (selectContext (pgContextId cId))
  case maybeContext of
    Nothing -> nodeError (NoContextFound cId)
    Just  r -> pure $ context2node r

queryContextSearchTable :: Select ContextSearchRead
queryContextSearchTable = selectTable contextTableSearch

selectContext :: Column SqlInt4 -> Select ContextRead
selectContext id' = proc () -> do
    row      <- queryContextTable -< ()
    restrict -< _context_id row .== id'
    returnA  -< row

runGetContexts :: Select ContextRead -> DBCmd err [Context HyperdataAny]
runGetContexts = runOpaQuery

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | order by publication date
-- Favorites (Bool), node_ngrams
selectContextsWith :: HasDBid NodeType
                => ParentId     -> Maybe NodeType
                -> Maybe Offset -> Maybe Limit   -> Select ContextRead
selectContextsWith parentId maybeContextType maybeOffset maybeLimit =
        --offset' maybeOffset $ limit' maybeLimit $ orderBy (asc (hyperdataDocument_Publication_date . node_hyperdata)) $ selectNodesWith' parentId typeId
  limit' maybeLimit $ offset' maybeOffset
                    $ orderBy (asc _context_id)
                    $ selectContextsWith' parentId maybeContextType

selectContextsWith' :: HasDBid NodeType
                 => ParentId -> Maybe NodeType -> Select ContextRead
selectContextsWith' parentId maybeContextType = proc () -> do
    context' <- (proc () -> do
      row@(Context _ _ typeId _ parentId' _ _ _) <- queryContextTable -< ()
      restrict -< parentId' .== (pgNodeId parentId)

      let typeId' = maybe 0 toDBid maybeContextType

      restrict -< if typeId' > 0
                     then typeId   .== (sqlInt4 (typeId' :: Int))
                     else (sqlBool True)
      returnA  -< row ) -< ()
    returnA -< context'


------------------------------------------------------------------------
getDocumentsV3WithParentId :: HasDBid NodeType => NodeId -> DBCmd err [Context HyperdataDocumentV3]
getDocumentsV3WithParentId n = runOpaQuery $ selectContextsWith' n (Just NodeDocument)

-- TODO: merge with getDocumentsWithParentId by having a class IsHyperdataDocument
getDocumentsWithParentId :: HasDBid NodeType => NodeId -> DBCmd err [Context HyperdataDocument]
getDocumentsWithParentId n = runOpaQuery $ selectContextsWith' n (Just NodeDocument)

------------------------------------------------------------------------
selectContextsWithParentID :: NodeId -> Select ContextRead
selectContextsWithParentID n = proc () -> do
    row@(Context _ _ _ _ parent_id _ _ _) <- queryContextTable -< ()
    restrict -< parent_id .== (pgNodeId n)
    returnA -< row


------------------------------------------------------------------------
-- | Example of use:
-- runCmdReplEasy  (getNodesWithType NodeList (Proxy :: Proxy HyperdataList))
getContextsWithType :: (HasNodeError err, JSONB a, HasDBid NodeType)
                    => NodeType -> proxy a -> DBCmd err [Context a]
getContextsWithType nt _ = runOpaQuery $ selectContextsWithType nt
  where
    selectContextsWithType ::  HasDBid NodeType
                         => NodeType -> Select ContextRead
    selectContextsWithType nt' = proc () -> do
        row@(Context _ _ tn _ _ _ _ _) <- queryContextTable -< ()
        restrict -< tn .== (sqlInt4 $ toDBid nt')
        returnA -< row

getContextsIdWithType :: (HasNodeError err, HasDBid NodeType)
                      => NodeType -> DBCmd err [ContextId]
getContextsIdWithType nt = do
  ns <- runOpaQuery $ selectContextsIdWithType nt
  pure (map UnsafeMkContextId ns)

selectContextsIdWithType :: HasDBid NodeType
                      => NodeType -> Select (Column SqlInt4)
selectContextsIdWithType nt = proc () -> do
    row@(Context _ _ tn _ _ _ _ _) <- queryContextTable -< ()
    restrict -< tn .== (sqlInt4 $ toDBid nt)
    returnA -< _context_id row

------------------------------------------------------------------------
