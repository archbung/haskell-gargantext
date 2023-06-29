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
  , Corpus.RawQuery(..)
  , Corpus.Limit(..)
  , GetCorpusError(..)
  , get
  , externalAPIs
  ) where

import Conduit
import Data.Bifunctor
import Data.Either (Either(..))
import Data.Maybe
import qualified Data.Text as T
import Gargantext.API.Admin.Orchestrator.Types (ExternalAPIs(..), externalAPIs)
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude
import qualified Gargantext.Core.Text.Corpus.API.Arxiv   as Arxiv
import qualified Gargantext.Core.Text.Corpus.API.Hal     as HAL
import qualified Gargantext.Core.Text.Corpus.API.Isidore as ISIDORE
import qualified Gargantext.Core.Text.Corpus.API.Istex   as ISTEX
import qualified Gargantext.Core.Text.Corpus.API.OpenAlex as OpenAlex
import qualified Gargantext.Core.Text.Corpus.API.Pubmed  as PUBMED
import qualified Gargantext.Core.Text.Corpus.Query as Corpus
import qualified PUBMED.Types as PUBMED
import Servant.Client (ClientError)

data GetCorpusError
  = -- | We couldn't parse the user input query into something meaningful.
    InvalidInputQuery !Corpus.RawQuery !T.Text
    -- | The external service returned an error.
  | ExternalAPIError !ClientError
  deriving (Show, Eq)

-- | Get External API metadata main function
get :: ExternalAPIs
    -> Lang
    -> Corpus.RawQuery
    -> Maybe PUBMED.APIKey
    -> Maybe Corpus.Limit
    -- -> IO [HyperdataDocument]
    -> IO (Either GetCorpusError (Maybe Integer, ConduitT () HyperdataDocument IO ()))
get externalAPI la q mPubmedAPIKey limit = do
  case Corpus.parseQuery q of
    Left err -> pure $ Left $ InvalidInputQuery q (T.pack err)
    Right corpusQuery -> case externalAPI of
      OpenAlex -> first ExternalAPIError <$>
                    OpenAlex.get (fromMaybe "" Nothing {- email -}) q limit
      PubMed -> first ExternalAPIError <$>
                  PUBMED.get (fromMaybe "" mPubmedAPIKey) corpusQuery limit
      --docs <- PUBMED.get   q default_limit -- EN only by default
      --pure (Just $ fromIntegral $ length docs, yieldMany docs)
      Arxiv   -> Right <$> Arxiv.get la corpusQuery limit
      HAL     -> first ExternalAPIError <$> HAL.getC  la (Corpus.getRawQuery q) (Corpus.getLimit <$> limit)
      IsTex   -> do docs <- ISTEX.get la (Corpus.getRawQuery q) (Corpus.getLimit <$> limit)
                    pure $ Right (Just $ fromIntegral $ length docs, yieldMany docs)
      Isidore -> do docs <- ISIDORE.get la (Corpus.getLimit <$> limit) (Just $ Corpus.getRawQuery q) Nothing
                    pure $ Right (Just $ fromIntegral $ length docs, yieldMany docs)
