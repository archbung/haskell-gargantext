{-# LANGUAGE LambdaCase #-}

module Gargantext.API.Errors (
    module Types
  , module Class

  -- * Conversion functions
  , backendErrorToFrontendError

  -- * Temporary shims
  , showAsServantJSONErr
  ) where

import Prelude

import Gargantext.API.Errors.Class as Class
import Gargantext.API.Errors.Types as Types
import Gargantext.Database.Query.Table.Node.Error
import Servant.Server
import qualified Data.Aeson as JSON
import qualified Network.HTTP.Types.Status as HTTP

_backendErrorTypeToErrStatus :: BackendErrorType -> HTTP.Status
_backendErrorTypeToErrStatus = \case
  BE_phylo_corpus_not_ready    -> HTTP.status500
  BE_node_not_found            -> HTTP.status500
  BE_tree_error_root_not_found -> HTTP.status404

-- | Transforms a backend internal error into something that the frontend
-- can consume. This is the only representation we offer to the outside world,
-- as we later encode this into a 'ServerError' in the main server handler.
backendErrorToFrontendError :: BackendInternalError -> FrontendError
backendErrorToFrontendError = \case
  InternalNodeError _nodeError
    -> undefined
  InternalTreeError _treeError
    -> undefined
  InternalValidationError _validationError
    -> undefined
  InternalJoseError _joseError
    -> undefined
  InternalServerError _internalServerError
    -> undefined
  InternalJobError _jobError
    -> undefined

showAsServantJSONErr :: BackendInternalError -> ServerError
showAsServantJSONErr (InternalNodeError err@(NoListFound {})) = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalNodeError err@NoRootFound) = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalNodeError err@NoCorpusFound) = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalNodeError err@NoUserFound{}) = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalNodeError err@(DoesNotExist {})) = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalServerError err) = err
showAsServantJSONErr a = err500 { errBody = JSON.encode a }
