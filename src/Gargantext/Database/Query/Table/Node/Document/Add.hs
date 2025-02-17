{-|
Module      : Gargantext.Database.Node.Document.Add
Description : Importing context of texts (documents)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Add Documents/Contact to a Corpus/Annuaire.

-}
------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeSynonymInstances #-}

------------------------------------------------------------------------
module Gargantext.Database.Query.Table.Node.Document.Add
  where

import Database.PostgreSQL.Simple (Query, Only(..))
import Database.PostgreSQL.Simple.SqlQQ ( sql )
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Gargantext.Database.Admin.Types.Node ( CorpusId, ContextId, NodeId, ParentId )
import Gargantext.Database.Prelude (runPGSQuery, formatPGSQuery, DBCmd)
import Gargantext.Prelude

---------------------------------------------------------------------------

add :: CorpusId -> [ContextId] -> DBCmd err [Only Int]
add pId ns = runPGSQuery queryAdd (Only $ Values fields inputData)
  where
    fields    = map (\t-> QualifiedIdentifier Nothing t) inputSqlTypes
    inputData = prepare pId ns

-- | Adds a single document. Useful for debugging purposes, but
-- not as efficient as adding documents in bulk via 'add'.
add_one :: CorpusId -> ContextId -> DBCmd err [Only Int]
add_one pId ctxId = runPGSQuery queryAdd (Only $ Values fields [InputData pId ctxId])
  where
    fields    = map (\t-> QualifiedIdentifier Nothing t) inputSqlTypes

add_debug :: CorpusId -> [ContextId] -> DBCmd err ByteString
add_debug pId ns = formatPGSQuery queryAdd (Only $ Values fields inputData)
  where
    fields    = map (\t-> QualifiedIdentifier Nothing t) inputSqlTypes
    inputData = prepare pId ns


-- | Input Tables: types of the tables
inputSqlTypes :: [Text]
inputSqlTypes = ["int4","int4","int4","int4"]

-- | SQL query to add documents
-- TODO return id of added documents only
queryAdd :: Query
queryAdd = [sql|
       WITH input_rows(node_id,context_id,score,category) AS (?)
       INSERT INTO nodes_contexts (node_id, context_id,score,category)
       SELECT * FROM input_rows
       ON CONFLICT (node_id, context_id) DO NOTHING -- on unique index
       RETURNING 1
       ;
           |]

prepare :: ParentId -> [ContextId] -> [InputData]
prepare pId ns = map (\cId -> InputData pId cId) ns

------------------------------------------------------------------------
-- * Main Types used

data InputData = InputData { inNode_id    :: NodeId
                           , inContext_id :: ContextId
                           } deriving (Show, Generic, Typeable)

instance ToRow InputData where
  toRow inputData = [ toField (inNode_id    inputData)
                    , toField (inContext_id inputData)
                    , toField (0 :: Int)
                    , toField (1 :: Int)
                    ]
