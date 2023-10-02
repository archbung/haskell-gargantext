
module Gargantext.API.GraphQL.Types where

import Data.Morpheus.Types
import Gargantext.API.Prelude

type GqlM e env = Resolver QUERY e (GargM env GargError)
type GqlM' e env a = ResolverM e (GargM env GargError) a
