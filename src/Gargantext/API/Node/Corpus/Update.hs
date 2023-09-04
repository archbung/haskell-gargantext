{-# LANGUAGE TypeApplications #-}
module Gargantext.API.Node.Corpus.Update
  ( addLanguageToCorpus )
  where

import Control.Lens
import Control.Monad
import Data.Proxy
import Gargantext.Core
import Gargantext.Database.Admin.Types.Hyperdata.Corpus
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude
import Gargantext.Utils.Jobs

-- | Updates the 'HyperdataCorpus' with the input 'Lang'.
addLanguageToCorpus :: (HasNodeError err, DbCmd' env err m, MonadJobStatus m)
                    => CorpusId
                    -> Lang
                    -> m ()
addLanguageToCorpus cId lang = do
  hyperNode <- getNodeWith cId (Proxy @HyperdataCorpus)
  let hyperNode' = hyperNode & over node_hyperdata (\corpus -> corpus { _hc_lang = Just lang })
  void $ updateHyperdata cId $ hyperNode' ^. node_hyperdata
