{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

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

newtype SchoolsArgs
  = SchoolsArgs ()
    deriving stock (Generic)
    deriving anyclass (GQLType)

resolveSchools
  :: SchoolsArgs -> GqlM e env [School]
resolveSchools (SchoolsArgs ()) = pure $ schools
