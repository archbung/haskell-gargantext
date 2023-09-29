{-# LANGUAGE GADTs               #-}
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

  -- * Smart constructors
  , nodeOwner
  , nodeSuper
  ) where

import Control.Lens
import Gargantext.API.Admin.Auth.Types
import Gargantext.Core.Types
import Gargantext.Database.Action.User
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

data AccessResult
  = Allow
  | Deny ServerError

instance Semigroup AccessResult where
  Allow <> Allow        = Allow
  Allow <> Deny status  = Deny status
  Deny status <> Allow  = Deny status
  Deny status <> Deny _ = Deny status

instance Monoid AccessResult where
  mempty = Allow


enforce :: Applicative m => ServerError -> Bool -> m AccessResult
enforce errStatus p = pure $ if p then Allow else Deny errStatus

-- | An access policy manager for gargantext that governs how resources are accessed
-- and who is entitled to see what.
data AccessPolicyManager = AccessPolicyManager
  { runAccessPolicy :: AuthenticatedUser -> BoolExpr AccessCheck -> DBCmd GargError AccessResult }

data AccessCheck
  = AC_node_owner NodeId
  | AC_master_user NodeId

nodeOwner :: NodeId -> BoolExpr AccessCheck
nodeOwner = BConst . Positive . AC_node_owner

nodeSuper :: NodeId -> BoolExpr AccessCheck
nodeSuper = BConst . Positive . AC_master_user

check :: HasNodeError err => AuthenticatedUser -> AccessCheck -> DBCmd err AccessResult
check (AuthenticatedUser nodeId) = \case
  AC_node_owner requestedNodeId
    -> enforce err403 $ nodeId == requestedNodeId
  AC_master_user _requestedNodeId
    -> do
      masterUsername <- _gc_masteruser <$> view hasConfig
      masterNodeId   <- getUserId (UserName masterUsername)
      enforce err403 $ (NodeId masterNodeId) == nodeId

accessPolicyManager :: AccessPolicyManager
accessPolicyManager = AccessPolicyManager (\ur ac -> interpretPolicy ur ac)

interpretPolicy :: AuthenticatedUser -> BoolExpr AccessCheck -> DBCmd GargError AccessResult
interpretPolicy ur = \case
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

data PolicyChecked a

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
