{-|
Module      : Gargantext.Database.Schema.NodeNodeNgrams
Description : TODO: remove this module and table in database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.Database.Query.Table.ContextNodeNgrams2
  ( module Gargantext.Database.Schema.ContextNodeNgrams2
  , insertContextNodeNgrams2
  , queryContextNodeNgrams2Table
  )
  where

import Gargantext.Database.Schema.Prelude
import Gargantext.Database.Schema.ContextNodeNgrams2
import Gargantext.Database.Admin.Types.Node (pgContextId)
import Gargantext.Database.Prelude (mkCmd, DBCmd)
import Prelude


queryContextNodeNgrams2Table :: Query ContextNodeNgrams2Read
queryContextNodeNgrams2Table = selectTable contextNodeNgrams2Table

-- | Insert utils
insertContextNodeNgrams2 :: [ContextNodeNgrams2] -> DBCmd err Int
insertContextNodeNgrams2 = insertContextNodeNgrams2W
                     . map (\(ContextNodeNgrams2 n1 n2 w) ->
                              ContextNodeNgrams2 (pgContextId n1)
                                                 (sqlInt4  n2)
                                                 (sqlDouble w)
                           )

insertContextNodeNgrams2W :: [ContextNodeNgrams2Write] -> DBCmd err Int
insertContextNodeNgrams2W nnnw =
  mkCmd $ \c -> fromIntegral <$> runInsert_ c insertNothing
    where
      insertNothing = Insert { iTable = contextNodeNgrams2Table
                              , iRows  = nnnw
                              , iReturning = rCount
                              , iOnConflict = (Just DoNothing)
                              }
