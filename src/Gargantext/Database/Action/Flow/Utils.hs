{-|
Module      : Gargantext.Database.Flow.Utils
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Database.Action.Flow.Utils
    where

import Control.Lens ((^.))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as DM
import Gargantext.Core.Types (TermsCount)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (DBCmd)
import Gargantext.Database.Query.Table.ContextNodeNgrams
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Types
import Gargantext.Prelude
import Gargantext.Core (toDBid)


data DocumentIdWithNgrams a b =
     DocumentIdWithNgrams
     { documentWithId :: Indexed NodeId a
     , documentNgrams :: HashMap b (Map NgramsType Int, TermsCount)
     } deriving (Show)

insertDocNgrams :: ListId
                -> HashMap (Indexed NgramsId Ngrams) (Map NgramsType (Map DocId (Int, TermsCount)))
                -> DBCmd err Int
insertDocNgrams lId m = do
  -- printDebug "[insertDocNgrams] ns" ns
  insertContextNodeNgrams ns
  where
    ns = [ ContextNodeNgrams (nodeId2ContextId docId)
                             lId (ng^.index)
                             (NgramsTypeId $ toDBid t)
                             (fromIntegral i)
                             cnt
         | (ng, t2n2i)       <- HashMap.toList m
         , (t,  n2i)         <- DM.toList t2n2i
         , (docId, (i, cnt)) <- DM.toList n2i
         ]

-- [(NodeId, {Ngrams: ({NgramsType: Int}, TermsCount)})]
-- {Ngrams: {NgramsType: {NodeId: (Int, TermsCount)}}}
