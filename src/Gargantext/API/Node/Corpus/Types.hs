{-# LANGUAGE TemplateHaskell    #-}

module Gargantext.API.Node.Corpus.Types where

import Control.Lens hiding (elements, Empty)
import Control.Monad.Fail (fail)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Monoid (mempty)
import Data.Swagger
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Regex.TDFA ((=~))

import Protolude ((++))
import Gargantext.Prelude

import qualified Gargantext.API.Admin.Orchestrator.Types as Types
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
instance ToSchema Database

database2origin :: Database -> DataOrigin
database2origin Empty   = InternalOrigin Types.IsTex
database2origin PubMed  = ExternalOrigin Types.PubMed
database2origin Arxiv   = ExternalOrigin Types.Arxiv
database2origin HAL     = ExternalOrigin Types.HAL
database2origin IsTex   = ExternalOrigin Types.IsTex
database2origin Isidore = ExternalOrigin Types.Isidore

------------------------------------------------------------------------
data Datafield = Gargantext
               | External (Maybe Database)
               | Web
               | Files
  deriving (Eq, Show, Generic)

instance FromJSON Datafield where
  parseJSON = withText "Datafield" $ \text ->
    case text of
      "Gargantext" -> pure Gargantext
      "Web" -> pure Web
      "Files" -> pure Files
      v ->
        let (preExternal, _, postExternal) = v =~ ("External " :: Text) :: (Text, Text, Text)
        in
        if preExternal == "" then do
          db <- parseJSON $ String postExternal
          pure $ External db
        else fail $ "Cannot match patterh 'External <db>' for string " ++ (T.unpack v)
instance ToJSON Datafield where
  toJSON (External db) = toJSON $ "External " ++ (show db)
  toJSON s = toJSON $ show s
instance ToSchema Datafield where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Datafield") $ mempty
      & type_ ?~ SwaggerObject
