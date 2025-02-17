{-|
Module      : Gargantext.Database
Description : Tools for Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

All Database related stuff here.

Target: just import this module and nothing else to work with
Gargantext's database.

Database Schema:
https://dl.gargantext.org/2023-06-09-gargantext-db-graph.svg

-}


module Gargantext.Database ( module Gargantext.Database.Prelude
                           , module Gargantext.Database.Schema.NodeNode
                           , insertDB
                        -- , module Gargantext.Database.Bashql
                           )
    where

import Gargantext.Prelude
import Gargantext.Database.Prelude (DBCmd) -- (connectGargandb)

-- import Gargantext.Database.Schema.Node
-- import Gargantext.Database.Query.Table.Node

import Gargantext.Database.Schema.NodeNode -- (NodeNode(..))
import Gargantext.Database.Query.Table.NodeNode


class InsertDB a where
  insertDB :: a -> DBCmd err Int

{-
class DeleteDB a where
  deleteDB :: a -> DBCmd err Int
-}

instance InsertDB [NodeNode] where
  insertDB = insertNodeNode


{-
instance InsertDB [Node a] where
  insertDB = insertNodes'

instance InsertDB [NodeNodeNgram] where
  insertDB = ...


-}
