{-# LANGUAGE TypeApplications #-}
module Gargantext.API.Node.Corpus.Update
  ( addLanguageToCorpus )
  where

import Gargantext.Core
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Admin.Types.Node
import Gargantext.Prelude
import Gargantext.Utils.Jobs
import Gargantext.Database.Admin.Types.Hyperdata.Corpus
import Gargantext.Database.Query.Table.Node
import Data.Proxy
import Control.Lens
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Control.Monad

-- | Updates the 'HyperdataCorpus' with the input 'Lang'.
addLanguageToCorpus :: (FlowCmdM env err m, MonadJobStatus m)
                    => CorpusId
                    -> Lang
                    -> m ()
addLanguageToCorpus cId lang = do
  hyperNode <- getNodeWith cId (Proxy @HyperdataCorpus)
  let hyperNode' = hyperNode & over node_hyperdata (\corpus -> corpus { _hc_lang = lang })
  void $ updateHyperdata cId hyperNode'
