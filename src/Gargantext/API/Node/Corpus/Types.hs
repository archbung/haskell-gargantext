{-# LANGUAGE TemplateHaskell    #-}

module Gargantext.API.Node.Corpus.Types where

import Control.Lens hiding (elements, Empty)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Monoid (mempty)
import Data.Swagger
import GHC.Generics (Generic)

import Gargantext.Prelude

import qualified Gargantext.API.Admin.Orchestrator.Types as T
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Action.Flow (DataOrigin(..))

data Database = Empty
              | PubMed
              | Arxiv
              | HAL
              | IsTex
              | Isidore
  deriving (Eq, Show, Generic, Enum, Bounded)

deriveJSON (unPrefix "") ''Database
instance ToSchema Database where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

database2origin :: Database -> DataOrigin
database2origin Empty   = InternalOrigin T.IsTex
database2origin PubMed  = ExternalOrigin T.PubMed
database2origin Arxiv   = ExternalOrigin T.Arxiv
database2origin HAL     = ExternalOrigin T.HAL
database2origin IsTex   = ExternalOrigin T.IsTex
database2origin Isidore = ExternalOrigin T.Isidore

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
--       "Gargantext" -> pure Gargantext
--       "Web" -> pure Web
--       "Files" -> pure Files
--       v ->
--         let (preExternal, _, postExternal) = v =~ ("External " :: Text) :: (Text, Text, Text)
--         in
--         if preExternal == "" then do
--           db <- parseJSON $ String postExternal
--           pure $ External db
--         else fail $ "Cannot match patterh 'External <db>' for string " ++ (T.unpack v)
-- instance ToJSON Datafield where
--   toJSON (External db) = toJSON $ "External " ++ (show db)
--   toJSON s = toJSON $ show s
instance ToSchema Datafield where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Datafield") $ mempty
      & type_ ?~ SwaggerObject
