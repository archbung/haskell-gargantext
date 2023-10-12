{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.IMT
  ( School(..)
  , SchoolsArgs(..)
  , resolveSchools
  )
  where

import Data.Morpheus.Types (GQLType)
import Gargantext.API.GraphQL.Types
import Gargantext.Core.Ext.IMT (School(..), schools)
import Gargantext.Prelude

data SchoolsArgs
  = SchoolsArgs
    { } deriving (Generic, GQLType)

resolveSchools
  :: SchoolsArgs -> GqlM e env [School]
resolveSchools SchoolsArgs { } = pure $ schools
