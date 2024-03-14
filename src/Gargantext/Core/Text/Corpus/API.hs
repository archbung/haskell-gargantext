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
import Control.Monad.Except
import Data.Text qualified as T
import EPO.API.Client.Types qualified as EPO
import Gargantext.API.Admin.Orchestrator.Types (ExternalAPIs(..), externalAPIs)
import Gargantext.Core (Lang(..), toISO639)
import Gargantext.Core.Text.Corpus.API.Arxiv qualified as Arxiv
import Gargantext.Core.Text.Corpus.API.EPO qualified as EPO
import Gargantext.Core.Text.Corpus.API.Hal qualified as HAL
import Gargantext.Core.Text.Corpus.API.Isidore qualified as ISIDORE
import Gargantext.Core.Text.Corpus.API.Istex qualified as ISTEX
import Gargantext.Core.Text.Corpus.API.OpenAlex qualified as OpenAlex
import Gargantext.Core.Text.Corpus.API.Pubmed qualified as PUBMED
import Gargantext.Core.Text.Corpus.Query qualified as Corpus
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude hiding (get)
import PUBMED.Types qualified as PUBMED
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
    -- ^ A user-selected language in which documents needs to be retrieved.
    -- If the provider doesn't support the search filtered by language, or if the language
    -- is not important, the frontend will simply send 'EN' to the backend.
    -> Corpus.RawQuery
    -> Maybe PUBMED.APIKey
    -> Maybe EPO.AuthKey
    -> Text
    -> Maybe Corpus.Limit
    -- -> IO [HyperdataDocument]
    -> IO (Either GetCorpusError (Maybe Integer, ConduitT () HyperdataDocument IO ()))
get externalAPI lang q mPubmedAPIKey epoAuthKey epoAPIUrl limit = do
  -- For PUBMED, HAL, IsTex, Isidore and OpenAlex, we want to send the query as-it.
  -- For Arxiv we parse the query into a structured boolean query we submit over.
  case externalAPI of
      PubMed   ->
        first ExternalAPIError <$> PUBMED.get (fromMaybe "" mPubmedAPIKey) q limit
      OpenAlex ->
        first ExternalAPIError <$> OpenAlex.get (fromMaybe "" Nothing {- email -}) q (Just $ toISO639 lang) limit
      Arxiv    -> runExceptT $ do
        corpusQuery <- ExceptT (pure parse_query)
        ExceptT $ fmap Right (Arxiv.get lang corpusQuery limit)
      HAL      ->
        first ExternalAPIError <$> HAL.getC (Just $ toISO639 lang) (Corpus.getRawQuery q) (Corpus.getLimit <$> limit)
      IsTex    -> do
        docs <- ISTEX.get lang (Corpus.getRawQuery q) (Corpus.getLimit <$> limit)
        pure $ Right (Just $ fromIntegral $ length docs, yieldMany docs)
      Isidore  -> do
        docs <- ISIDORE.get lang (Corpus.getLimit <$> limit) (Just $ Corpus.getRawQuery q) Nothing
        pure $ Right (Just $ fromIntegral $ length docs, yieldMany docs)
      EPO -> do
        first ExternalAPIError <$> EPO.get epoAuthKey epoAPIUrl q (toISO639 lang) limit
  where
    parse_query = first (InvalidInputQuery q . T.pack) $ Corpus.parseQuery q
