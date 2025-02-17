{-|
Module      : Gargantext.Database.Node.Update
Description : Update Node in Database (Postgres)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Database.Query.Table.Node.Update (Update(..), update)
  where

import Data.Text qualified as DT
import Database.PostgreSQL.Simple ( Only(Only) )
import Gargantext.Core.Types (Name)
import Gargantext.Database.Admin.Types.Node (NodeId, ParentId)
import Gargantext.Database.Prelude (DBCmd, runPGSQuery)
import Gargantext.Prelude

-- import Data.ByteString
--rename :: NodeId -> Text -> IO ByteString
--rename nodeId name = formatPGSQuery "UPDATE nodes SET name=? where id=?" (name,nodeId)
------------------------------------------------------------------------

data Update = Rename NodeId Name
            | Move   NodeId ParentId

-- | Update a Node
-- TODO : Field as parameter
-- TODO jsonb values, consider this: 
-- https://stackoverflow.com/questions/26703476/how-to-perform-update-operations-on-columns-of-type-jsonb-in-postgres-9-4

unOnly :: Only a -> a
unOnly (Only a) = a

-- TODO-ACCESS
update :: Update -> DBCmd err [Int]
update (Rename nId name) = map unOnly <$> runPGSQuery "UPDATE nodes SET name=? where id=? returning id"
                                           (DT.take 255 name,nId)
update (Move nId pId)    = map unOnly <$> runPGSQuery "UPDATE nodes SET parent_id= ? where id=? returning id"
                                           (pId, nId)


