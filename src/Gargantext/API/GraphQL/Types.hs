
module Gargantext.API.GraphQL.Types where

import Data.Morpheus.Types
import Gargantext.API.Prelude
import Gargantext.API.Errors.Types

type GqlM e env = Resolver QUERY e (GargM env BackendInternalError)
type GqlM' e env a = ResolverM e (GargM env BackendInternalError) a
