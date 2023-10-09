{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Gargantext.API.Auth.PolicyCheck (
    AccessCheck(..)
  , AccessResult(..)
  , AccessPolicyManager(..)
  , PolicyChecked
  , BoolExpr(..)

  -- * Smart constructors for access checks
  , nodeDescendant
  , nodeSuper
  , nodeUser
  , nodeChecks
  , alwaysAllow
  , alwaysDeny
  ) where

import Control.Lens
import Gargantext.API.Admin.Auth.Types
import Gargantext.Core.Types
import Gargantext.Database.Prelude (DBCmd, HasConfig (..))
import Gargantext.Prelude.Config (GargConfig(..))
import Prelude
import Servant
import Servant.Ekg
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import qualified Servant.Swagger as Swagger
import Gargantext.Core.Types.Individu
import Gargantext.Database.Query.Table.Node.Error
import Data.BoolExpr
import Control.Monad
import Gargantext.API.Prelude
import Servant.Auth.Server.Internal.AddSetCookie
import Gargantext.Database.Query.Tree
import Gargantext.Database.Query.Tree.Root

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Phantom type that allows us to embellish a Servant route with a policy check.
data PolicyChecked a

-- | The result of an access check.
data AccessResult
  = -- | Grants access.
    Allow
    -- | Denies access with the given 'ServerError'.
  | Deny ServerError

instance Semigroup AccessResult where
  Allow <> Allow        = Allow
  Allow <> Deny status  = Deny status
  Deny status <> Allow  = Deny status
  Deny status <> Deny _ = Deny status

instance Monoid AccessResult where
  mempty = Allow

-- | An access policy manager for gargantext that governs how resources are accessed
-- and who is entitled to see what.
data AccessPolicyManager = AccessPolicyManager
  { runAccessPolicy :: AuthenticatedUser -> BoolExpr AccessCheck -> DBCmd GargError AccessResult }

-- | A type representing all the possible access checks we might want to perform on a resource,
-- typically a 'Node'.
data AccessCheck
  = -- | Grants access if the input 'NodeId' is a descendant of the
    -- one for the logged-in user.
    AC_node_descendant  NodeId
    -- | Grants access if the input 'NodeId' /is/ the logged-in user.
  | AC_user_node        NodeId
    -- | Grants access if the logged-in user is the master user.
  | AC_master_user      NodeId
    -- | Always grant access, effectively a public route.
  | AC_always_allow
    -- | Always denies access.
  | AC_always_deny
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Running access checks
-------------------------------------------------------------------------------

-- | The static access manager returned as part of a 'Servant' handler every time
-- we use the 'PolicyChecked' combinator.
accessPolicyManager :: AccessPolicyManager
accessPolicyManager = AccessPolicyManager (\ur ac -> interpretPolicy ur ac)
  where
    interpretPolicy :: AuthenticatedUser -> BoolExpr AccessCheck -> DBCmd GargError AccessResult
    interpretPolicy ur chk = case chk of
      BAnd b1 b2
        -> liftM2 (<>) (interpretPolicy ur b1) (interpretPolicy ur b2)
      BOr b1 b2
        -> do
          c1 <- interpretPolicy ur b1
          case c1 of
            Allow  -> pure Allow
            Deny{} -> interpretPolicy ur b2
      BNot b1
        -> do
          res <- interpretPolicy ur b1
          case res of
            Allow  -> pure $ Deny err403
            Deny _ -> pure Allow
      BTrue
        -> pure Allow
      BFalse
        -> pure $ Deny err403
      BConst (Positive b)
        -> check ur b
      BConst (Negative b)
        -> check ur b


check :: HasNodeError err => AuthenticatedUser -> AccessCheck -> DBCmd err AccessResult
check (AuthenticatedUser loggedUserNodeId _loggedUserUserId) = \case
  AC_always_deny
    -> pure $ Deny err500
  AC_always_allow
    -> pure Allow
  AC_user_node requestedNodeId
    -> enforce err403 $ loggedUserNodeId == requestedNodeId
  AC_master_user _requestedNodeId
    -> do
      masterUsername <- _gc_masteruser <$> view hasConfig
      masterNodeId   <- getRootId (UserName masterUsername)
      enforce err403 $ masterNodeId == loggedUserNodeId
  AC_node_descendant nodeId
    -> enforce err403 =<< nodeId `isDescendantOf` loggedUserNodeId

-------------------------------------------------------------------------------
-- Smart constructors of access checks
-------------------------------------------------------------------------------

nodeUser :: NodeId -> BoolExpr AccessCheck
nodeUser = BConst . Positive . AC_user_node

nodeSuper :: NodeId -> BoolExpr AccessCheck
nodeSuper = BConst . Positive . AC_master_user

nodeDescendant :: NodeId -> BoolExpr AccessCheck
nodeDescendant = BConst . Positive . AC_node_descendant

-- FIXME(adinapoli) Checks temporarily disabled.
nodeChecks :: NodeId -> BoolExpr AccessCheck
nodeChecks _nid = alwaysAllow
  where
    _disabled = nodeUser _nid `BOr` nodeSuper _nid `BOr` nodeDescendant _nid

alwaysAllow :: BoolExpr AccessCheck
alwaysAllow = BConst . Positive $ AC_always_allow

alwaysDeny :: BoolExpr AccessCheck
alwaysDeny = BConst . Positive $ AC_always_deny

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance (HasServer subApi ctx) => HasServer (PolicyChecked subApi) ctx where
  type ServerT (PolicyChecked subApi) m = AccessPolicyManager -> ServerT subApi m
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy subApi) pc nt . s
  route Proxy ctx d = route (Proxy :: Proxy subApi) ctx (d `addParameterCheck` getStockAccessPolicy)
    where
      getStockAccessPolicy :: DelayedIO AccessPolicyManager
      getStockAccessPolicy = DelayedIO $ pure accessPolicyManager

type instance AddSetCookieApi (PolicyChecked a) = AddSetCookieApi a

instance AddSetCookies ('S n) old new => AddSetCookies ('S n) (AccessPolicyManager -> old) new where
  addSetCookies lst old = addSetCookies lst (old accessPolicyManager)

instance Swagger.HasSwagger sub => Swagger.HasSwagger (PolicyChecked sub) where
    toSwagger _ = Swagger.toSwagger (Proxy :: Proxy sub)

instance HasEndpoint sub => HasEndpoint (PolicyChecked sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | If the given predicate holds then grant access, otherwise denies access
-- with the given 'ServerError'.
enforce :: Applicative m => ServerError -> Bool -> m AccessResult
enforce errStatus p = pure $ if p then Allow else Deny errStatus
