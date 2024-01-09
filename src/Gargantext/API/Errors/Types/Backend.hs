{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}

module Gargantext.API.Errors.Types.Backend where

import Data.Aeson
import Data.Kind
import Data.Singletons.TH
import Data.Typeable
import Gargantext.Utils.Dict
import Prelude

-- | A (hopefully and eventually) exhaustive list of backend errors.
data BackendErrorCode
  =
  -- node errors
    EC_404__node_list_not_found
  | EC_404__node_root_not_found
  | EC_404__node_lookup_failed_not_found
  | EC_404__node_lookup_failed_parent_not_found
  | EC_400__node_lookup_failed_user_too_many_roots
  | EC_404__node_lookup_failed_user_not_found
  | EC_404__node_lookup_failed_username_not_found
  | EC_404__node_corpus_not_found
  | EC_500__node_not_implemented_yet
  | EC_404__node_context_not_found
  | EC_400__node_creation_failed_no_parent
  | EC_400__node_creation_failed_parent_exists
  | EC_400__node_creation_failed_insert_node
  | EC_400__node_creation_failed_user_negative_id
  | EC_500__node_generic_exception
  | EC_400__node_needs_configuration
  -- validation errors
  | EC_400__validation_error
  -- authentication errors
  | EC_403__login_failed_error
  | EC_403__login_failed_invalid_username_or_password
  -- tree errors
  | EC_404__tree_root_not_found
  | EC_404__tree_empty_root
  | EC_500__tree_too_many_roots
  -- internal server errors
  | EC_500__internal_server_error
  | EC_405__not_allowed
  -- job errors
  | EC_500__job_invalid_id_type
  | EC_500__job_expired
  | EC_500__job_invalid_mac
  | EC_500__job_unknown_job
  | EC_500__job_generic_exception
  deriving (Show, Read, Eq, Enum, Bounded)

$(genSingletons [''BackendErrorCode])

----------------------------------------------------------------------------
-- This data family maps a 'BackendErrorCode' into a concrete payload.
----------------------------------------------------------------------------
data family ToFrontendErrorData (payload :: BackendErrorCode) :: Type

class ( SingI payload
      , ToJSON (ToFrontendErrorData payload)
      , FromJSON (ToFrontendErrorData payload)
      , Show (ToFrontendErrorData payload)
      , Eq (ToFrontendErrorData payload)
      , Typeable payload
      ) => IsFrontendErrorData payload where
  isFrontendErrorData :: Proxy payload -> Dict IsFrontendErrorData payload
