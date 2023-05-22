{-|
Module      : Gargantext.Core.Text.Corpus.API
Description : All crawlers of Gargantext in one file.
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Core.Text.Corpus.API
  ( ExternalAPIs(..)
  , Query
  , Limit
  , get
  , externalAPIs
  ) where

import Conduit
import Control.Lens ((^.))
import Data.Either (Either(..))
import Data.Maybe
import Gargantext.API.Admin.Orchestrator.Types (ExternalAPIs(..), externalAPIs)
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig, gc_pubmed_api_key)
import qualified Gargantext.Core.Text.Corpus.API.Arxiv   as Arxiv
import qualified Gargantext.Core.Text.Corpus.API.Hal     as HAL
import qualified Gargantext.Core.Text.Corpus.API.Isidore as ISIDORE
import qualified Gargantext.Core.Text.Corpus.API.Istex   as ISTEX
import qualified Gargantext.Core.Text.Corpus.API.Pubmed  as PUBMED
import Servant.Client (ClientError)

-- | Get External API metadata main function
get :: GargConfig
    -> ExternalAPIs
    -> Lang
    -> Query
    -> Maybe Limit
    -- -> IO [HyperdataDocument]
    -> IO (Either ClientError (Maybe Integer, ConduitT () HyperdataDocument IO ()))
get cfg PubMed _la q limit = PUBMED.get (cfg ^. gc_pubmed_api_key) q limit
  --docs <- PUBMED.get   q default_limit -- EN only by default
  --pure (Just $ fromIntegral $ length docs, yieldMany docs)
get _ Arxiv    la q limit = Arxiv.get la q (fromIntegral <$> limit)
get _ HAL      la q limit = HAL.getC  la q limit
get _ IsTex    la q limit = do
  docs <- ISTEX.get la q limit
  pure $ Right (Just $ fromIntegral $ length docs, yieldMany docs)
get _ Isidore  la q limit = do
  docs <- ISIDORE.get la (fromIntegral <$> limit) (Just q) Nothing
  pure $ Right (Just $ fromIntegral $ length docs, yieldMany docs)
get _ externalApi  _  _ _ = panic $ "[G.C.T.Corpus.API] This options are note taken into account: " <>  (cs $ show externalApi)

-- | Some Sugar for the documentation
type Query = PUBMED.Query
type Limit = PUBMED.Limit
