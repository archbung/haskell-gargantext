{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Gargantext.API.Errors (
    module Types
  , module Class

  -- * Conversion functions
  , backendErrorToFrontendError
  , frontendErrorToServerError

  -- * Temporary shims
  , showAsServantJSONErr
  ) where

import Prelude

import Gargantext.API.Errors.Class as Class
import Gargantext.API.Errors.Types as Types
import Gargantext.API.Errors.TH (deriveHttpStatusCode)
import Gargantext.Database.Query.Table.Node.Error hiding (nodeError)
import Servant.Server
import qualified Data.Aeson as JSON
import qualified Network.HTTP.Types.Status as HTTP
import qualified Data.Text as T

$(deriveHttpStatusCode ''BackendErrorCode)

-- | Transforms a backend internal error into something that the frontend
-- can consume. This is the only representation we offer to the outside world,
-- as we later encode this into a 'ServerError' in the main server handler.
backendErrorToFrontendError :: BackendInternalError -> FrontendError
backendErrorToFrontendError = \case
  InternalNodeError nodeError
    -> nodeErrorToFrontendError nodeError
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

nodeErrorToFrontendError :: NodeError -> FrontendError
nodeErrorToFrontendError ne = case ne of
  NoListFound lid
    -> mkFrontendErrShow $ FE_node_error_list_not_found lid
  NoRootFound
    -> mkFrontendErrShow FE_node_error_root_not_found
  NoCorpusFound
    -> mkFrontendErrShow FE_node_error_corpus_not_found
  NoUserFound _ur
    -> undefined
  MkNode
    -> undefined
  UserNoParent
    -> undefined
  HasParent
    -> undefined
  ManyParents
    -> undefined
  NegativeId
    -> undefined
  NotImplYet
    -> undefined
  ManyNodeUsers
    -> undefined
  DoesNotExist _nodeId
    -> undefined
  NoContextFound _contextId
    -> undefined
  NeedsConfiguration
    -> undefined
  NodeError _txt
    -> undefined
  QueryNoParse _txt
    -> undefined

-- | Converts a 'FrontendError' into a 'ServerError' that the servant app can
-- return to the frontend.
frontendErrorToServerError :: FrontendError -> ServerError
frontendErrorToServerError fe@(FrontendError diag ty _) =
  ServerError { errHTTPCode     = HTTP.statusCode $ backendErrorTypeToErrStatus ty
              , errReasonPhrase = T.unpack diag
              , errBody         = JSON.encode fe
              , errHeaders      = mempty
              }

showAsServantJSONErr :: BackendInternalError -> ServerError
showAsServantJSONErr (InternalNodeError err@(NoListFound {}))  = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalNodeError err@NoRootFound{})     = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalNodeError err@NoCorpusFound)     = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalNodeError err@NoUserFound{})     = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalNodeError err@(DoesNotExist {})) = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalServerError err)                 = err
showAsServantJSONErr a                                         = err500 { errBody = JSON.encode a }
