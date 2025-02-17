
module Gargantext.API.GraphQL.PolicyCheck where

import Prelude

import Control.Monad.Except
import Gargantext.API.Admin.Auth.Types
import Gargantext.API.Auth.PolicyCheck
import Gargantext.API.Errors.Types
import Gargantext.API.GraphQL.Types
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)

withPolicy :: (HasConnectionPool env, HasConfig env)
           => AuthenticatedUser
           -> AccessPolicyManager
           -> BoolExpr AccessCheck
           -> GqlM e env a
           -> GqlM e env a
withPolicy ur mgr checks m = case mgr of
  AccessPolicyManager{runAccessPolicy} -> do
    res <- lift $ runAccessPolicy ur checks
    case res of
      Allow     -> m
      Deny err  -> lift $ throwError $ InternalServerError $ err

