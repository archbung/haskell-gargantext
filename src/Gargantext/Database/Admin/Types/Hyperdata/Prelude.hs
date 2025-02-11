{-# LANGUAGE ConstraintKinds #-}

{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Prelude
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Database.Admin.Types.Hyperdata.Prelude
  ( module Control.Lens
  , module Data.Aeson
  , module Data.Aeson.TH
  , module Data.Aeson.Types
  , module Data.ByteString.Lazy.Internal
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Swagger
  , module Data.Text
  , module Database.PostgreSQL.Simple.FromField
  , module Database.PostgreSQL.Simple.ToField
  , module GHC.Generics
  , module Gargantext.Core.Utils.Prefix
  , module Gargantext.Database.Prelude
  , module Opaleye
  , module Test.QuickCheck
  , module Test.QuickCheck.Arbitrary
  , Hyperdata
  , HyperdataC
  , Chart(..)
  )
  where

import Control.Lens hiding (elements, (&), (.=), Indexed)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Swagger hiding (unwrapUnaryRecords, constructorTagModifier, allNullaryToStringTag, allOf, fieldLabelModifier)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToField (ToField, toField, toJSONField)
import GHC.Generics (Generic)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger, wellNamedSchema)
import Gargantext.Database.Prelude (fromField', JSONB)
import Gargantext.Prelude
import Opaleye (DefaultFromField, defaultFromField, Nullable, SqlJsonb, fromPGSFromField)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary hiding (vector)

------------------------------------------------------------------------
-- Only Hyperdata types should be member of this type class.
class Hyperdata a

type HyperdataC a = ( Hyperdata a
                    , JSONB a
                    , ToJSON a
                    , FromJSON a
                    , FromField a )


data Chart =
    CDocsHistogram
  | CAuthorsPie
  | CInstitutesTree
  | CTermsMetrics
  deriving (Generic, Show, Eq)
instance ToJSON Chart
instance FromJSON Chart
instance ToSchema Chart
