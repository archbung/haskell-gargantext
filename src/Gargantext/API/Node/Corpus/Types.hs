{-# LANGUAGE TemplateHaskell    #-}

module Gargantext.API.Node.Corpus.Types where

import Control.Lens hiding (elements, Empty)
import Control.Monad.Reader (MonadReader)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Monoid (mempty)
import Data.Swagger
import GHC.Generics (Generic)
import Test.QuickCheck
import qualified PUBMED.Types as PUBMED

import Gargantext.Prelude

import qualified Gargantext.API.Admin.Orchestrator.Types as Types
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Action.Flow (DataOrigin(..))
import Gargantext.Database.Prelude (HasConfig(..))

data Database = Empty
              | PubMed { _api_key :: Maybe PUBMED.APIKey }
              | Arxiv
              | HAL
              | IsTex
              | Isidore
  deriving (Eq, Show, Generic)

instance Arbitrary Database
  where
    arbitrary = elements [ Empty
                         , PubMed { _api_key = Nothing }
                         , Arxiv
                         , HAL
                         , IsTex
                         , Isidore ]


deriveJSON (unPrefix "") ''Database
instance ToSchema Database where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

database2origin :: ( MonadReader env m
                   , HasConfig env ) => Database -> m DataOrigin
database2origin Empty   = pure $ InternalOrigin Types.IsTex
database2origin (PubMed { _api_key }) = do
  -- pubmed_api_key <- view $ hasConfig . gc_pubmed_api_key

  pure $ ExternalOrigin $ Types.PubMed { mAPIKey = _api_key }
database2origin Arxiv   = pure $ ExternalOrigin Types.Arxiv
database2origin HAL     = pure $ ExternalOrigin Types.HAL
database2origin IsTex   = pure $ ExternalOrigin Types.IsTex
database2origin Isidore = pure $ ExternalOrigin Types.Isidore

------------------------------------------------------------------------
data Datafield = Gargantext
               | External Database
               | Web
               | Files
  deriving (Eq, Show, Generic)

instance FromJSON Datafield
instance ToJSON Datafield
-- instance FromJSON Datafield where
--   parseJSON = withText "Datafield" $ \text ->
--     case text of
--       "Gargantext"
--         -> pure Gargantext
--       "Web"
--         -> pure Web
--       "Files"
--         -> pure Files
--       v -> case T.breakOnEnd " " v of
--           ("External ", dbName)
--             -> External <$> parseJSON (String dbName)
--           _ -> fail $ "Cannot match patterh 'External <db>' for string " <> T.unpack v

-- instance ToJSON Datafield where
--   toJSON (External db) = toJSON $ "External " <> show db
--   toJSON s = toJSON $ show s

instance Arbitrary Datafield where
  arbitrary = oneof [pure Gargantext, pure Web, pure Files, External <$> arbitrary]

instance ToSchema Datafield where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Datafield") $ mempty
      & type_ ?~ SwaggerObject
