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
    EC_404__node_error_list_not_found
  | EC_404__node_error_root_not_found
  | EC_404__node_error_not_found
  | EC_404__node_error_corpus_not_found
  | EC_500__node_error_not_implemented_yet
  -- validation errors
  | EC_400__validation_error
  -- authentication errors
  | EC_403__login_failed_error
  -- tree errors
  | EC_404__tree_error_root_not_found
  | EC_404__tree_error_empty_root
  | EC_500__tree_error_too_many_roots
  -- internal server errors
  | EC_500__internal_server_error
  -- job errors
  | EC_500__job_error_invalid_id_type
  | EC_500__job_error_expired
  | EC_500__job_error_invalid_mac
  | EC_500__job_error_unknown_job
  | EC_500__job_error_generic_exception
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
