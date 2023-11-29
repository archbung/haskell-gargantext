{-|
Module      : Gargantext.API.Node.Corpus.Types
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE TemplateHaskell    #-}

module Gargantext.API.Node.Corpus.Types where

import Control.Lens hiding (elements, Empty)
import Control.Monad.Fail (fail)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Swagger
import Data.Text qualified as T
import Gargantext.API.Admin.Orchestrator.Types qualified as Types
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Action.Flow (DataOrigin(..))
import Gargantext.Prelude
import Test.QuickCheck

data Database = Empty
              | OpenAlex
              | PubMed
              | Arxiv
              | HAL
              | IsTex
              | Isidore
              | EPO
  deriving (Eq, Show, Generic, Enum, Bounded)

instance Arbitrary Database where
  arbitrary = arbitraryBoundedEnum

deriveJSON (unPrefix "") ''Database
instance ToSchema Database where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

database2origin :: Database -> DataOrigin
database2origin Empty    = InternalOrigin Types.IsTex
database2origin OpenAlex = ExternalOrigin Types.OpenAlex
database2origin PubMed   = ExternalOrigin Types.PubMed
database2origin Arxiv    = ExternalOrigin Types.Arxiv
database2origin HAL      = ExternalOrigin Types.HAL
database2origin IsTex    = ExternalOrigin Types.IsTex
database2origin Isidore  = ExternalOrigin Types.Isidore
database2origin EPO      = ExternalOrigin Types.EPO

------------------------------------------------------------------------
data Datafield = Gargantext
               | External Database
               | Web
               | Files
  deriving (Eq, Show, Generic)

instance FromJSON Datafield where
  parseJSON (String "Gargantext") = pure Gargantext
  parseJSON (String "Web") = pure Web
  parseJSON (String "Files") = pure Files
  parseJSON (Object o) = do
    db <- o .: "External"
    pure $ External db
  parseJSON x = withText "Datafield" (\text ->
    fail $ "Cannot match pattern '<db>' for string " <> T.unpack text) x

instance ToJSON Datafield where
  toJSON (External db) = toJSON $ object [ ("External", toJSON db) ]
  toJSON s = toJSON (show s :: Text)

instance Arbitrary Datafield where
  arbitrary = oneof [pure Gargantext, pure Web, pure Files, External <$> arbitrary]

instance ToSchema Datafield where
  declareNamedSchema _ = do
    pure $ NamedSchema (Just "Datafield") $ mempty
      & type_ ?~ SwaggerObject
