{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Gargantext.API.Errors (
    module Types
  , module Class

  -- * Types
  , GargErrorScheme(..)

  -- * Conversion functions
  , backendErrorToFrontendError
  , frontendErrorToServerError
  , frontendErrorToGQLServerError

  -- * Temporary shims
  , showAsServantJSONErr
  ) where

import Prelude

import Control.Exception
import Data.Aeson qualified as JSON
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TE
import Data.Validity ( prettyValidation )
import Gargantext.API.Admin.Auth.Types
import Gargantext.API.Errors.Class as Class
import Gargantext.API.Errors.TH (deriveHttpStatusCode)
import Gargantext.API.Errors.Types as Types
import Gargantext.Database.Query.Table.Node.Error hiding (nodeError)
import Gargantext.Database.Query.Tree hiding (treeError)
import Gargantext.Utils.Jobs.Monad (JobError(..))
import Network.HTTP.Types.Status qualified as HTTP
import Servant.Server

$(deriveHttpStatusCode ''BackendErrorCode)

data GargErrorScheme
  = -- | The old error scheme.
    GES_old
  -- | The new error scheme, that returns a 'FrontendError'.
  | GES_new
  -- | Error scheme for GraphQL, has to be slightly different
  --   {errors: [{message, extensions: { ... }}]}
  --   https://spec.graphql.org/June2018/#sec-Errors
    deriving (Show, Eq)

-- | Transforms a backend internal error into something that the frontend
-- can consume. This is the only representation we offer to the outside world,
-- as we later encode this into a 'ServerError' in the main server handler.
backendErrorToFrontendError :: BackendInternalError -> FrontendError
backendErrorToFrontendError = \case
  InternalAuthenticationError authError
    -> authErrorToFrontendError authError
  InternalNodeError nodeError
    -> nodeErrorToFrontendError nodeError
  InternalJobError jobError
    -> jobErrorToFrontendError jobError
  InternalServerError internalServerError
    -> internalServerErrorToFrontendError internalServerError
  InternalTreeError treeError
    -> treeErrorToFrontendError treeError
  -- As this carries a 'SomeException' which might exposes sensible
  -- information, we do not send to the frontend its content.
  InternalUnexpectedError _
    -> let msg = T.pack $ "An unexpected error occurred. Please check your server logs."
       in mkFrontendErr' msg $ FE_internal_server_error msg
  InternalValidationError validationError
    -> mkFrontendErr' "A validation error occurred"
         $ FE_validation_error $ case prettyValidation validationError of
             Nothing -> "unknown_validation_error"
             Just v  -> T.pack v

frontendErrorToGQLServerError :: FrontendError -> ServerError
frontendErrorToGQLServerError fe@(FrontendError diag ty _) =
  ServerError { errHTTPCode     = HTTP.statusCode $ backendErrorTypeToErrStatus ty
              , errReasonPhrase = T.unpack diag
              , errBody         = JSON.encode (GraphQLError fe)
              , errHeaders      = mempty
              }

authErrorToFrontendError :: AuthenticationError -> FrontendError
authErrorToFrontendError = \case
  -- For now, we ignore the Jose error, as they are too specific
  -- (i.e. they should be logged internally to Sentry rather than shared
  -- externally).
  LoginFailed nid uid _
    -> mkFrontendErr' "Invalid username/password, or invalid session token." $ FE_login_failed_error nid uid
  InvalidUsernameOrPassword
    -> mkFrontendErr' "Invalid username or password." $ FE_login_failed_invalid_username_or_password
  UserNotAuthorized uId msg
    -> mkFrontendErr' "User not authorized. " $ FE_user_not_authorized uId msg

-- | Converts a 'FrontendError' into a 'ServerError' that the servant app can
-- return to the frontend.
frontendErrorToServerError :: FrontendError -> ServerError
frontendErrorToServerError fe@(FrontendError diag ty _) =
  ServerError { errHTTPCode     = HTTP.statusCode $ backendErrorTypeToErrStatus ty
              , errReasonPhrase = T.unpack diag
              , errBody         = JSON.encode fe
              , errHeaders      = mempty
              }

internalServerErrorToFrontendError :: ServerError -> FrontendError
internalServerErrorToFrontendError = \case
  ServerError{..}
    | errHTTPCode == 405
    -> mkFrontendErr' (T.pack errReasonPhrase) $ FE_not_allowed (TL.toStrict $ TE.decodeUtf8 $ errBody)
    | otherwise
    -> mkFrontendErr' (T.pack errReasonPhrase) $ FE_internal_server_error (TL.toStrict $ TE.decodeUtf8 $ errBody)

jobErrorToFrontendError :: JobError -> FrontendError
jobErrorToFrontendError = \case
  InvalidIDType idTy -> mkFrontendErrNoDiagnostic $ FE_job_invalid_id_type idTy
  IDExpired jobId    -> mkFrontendErrNoDiagnostic $ FE_job_expired jobId
  InvalidMacID macId -> mkFrontendErrNoDiagnostic $ FE_job_invalid_mac macId
  UnknownJob jobId   -> mkFrontendErrNoDiagnostic $ FE_job_unknown_job jobId
  JobException err   -> mkFrontendErrNoDiagnostic $ FE_job_generic_exception (T.pack $ displayException err)

nodeErrorToFrontendError :: NodeError -> FrontendError
nodeErrorToFrontendError ne = case ne of
  NoListFound lid
    -> mkFrontendErrShow $ FE_node_list_not_found lid
  NoRootFound
    -> mkFrontendErrShow FE_node_root_not_found
  NoCorpusFound
    -> mkFrontendErrShow FE_node_corpus_not_found
  NoUserFound _ur
    -> undefined
  NodeCreationFailed reason
    -> case reason of
         UserParentAlreadyExists pId uId
           -> mkFrontendErrShow $ FE_node_creation_failed_parent_exists uId pId
         UserParentDoesNotExist uId
           -> mkFrontendErrShow $ FE_node_creation_failed_no_parent uId
         InsertNodeFailed uId pId
           -> mkFrontendErrShow $ FE_node_creation_failed_insert_node uId pId
         UserHasNegativeId uid
           -> mkFrontendErrShow $ FE_node_creation_failed_user_negative_id uid
  NodeLookupFailed reason
    -> case reason of
         NodeDoesNotExist nid
           -> mkFrontendErrShow $ FE_node_lookup_failed_not_found nid
         NodeParentDoesNotExist nid
           -> mkFrontendErrShow $ FE_node_lookup_failed_parent_not_found nid
         UserDoesNotExist uid
           -> mkFrontendErrShow $ FE_node_lookup_failed_user_not_found uid
         UserNameDoesNotExist uname
           -> mkFrontendErrShow $ FE_node_lookup_failed_username_not_found uname
         UserHasTooManyRoots uid roots
           -> mkFrontendErrShow $ FE_node_lookup_failed_user_too_many_roots uid roots
  NotImplYet
    -> mkFrontendErrShow FE_node_not_implemented_yet
  NoContextFound contextId
    -> mkFrontendErrShow $ FE_node_context_not_found contextId
  NeedsConfiguration
    -> mkFrontendErrShow $ FE_node_needs_configuration
  NodeError err
    -> mkFrontendErrShow $ FE_node_generic_exception (T.pack $ displayException err)

  -- backward-compatibility shims, to remove eventually.
  DoesNotExist nid
    -> mkFrontendErrShow $ FE_node_lookup_failed_not_found nid

treeErrorToFrontendError :: TreeError -> FrontendError
treeErrorToFrontendError te = case te of
  NoRoot             -> mkFrontendErrShow FE_tree_root_not_found
  EmptyRoot          -> mkFrontendErrShow FE_tree_empty_root
  TooManyRoots roots -> mkFrontendErrShow $ FE_tree_too_many_roots roots

showAsServantJSONErr :: BackendInternalError -> ServerError
showAsServantJSONErr (InternalNodeError err@(NoListFound {}))  = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalNodeError err@NoRootFound{})     = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalNodeError err@NoCorpusFound)     = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalNodeError err@NoUserFound{})     = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalNodeError err@(DoesNotExist {})) = err404 { errBody = JSON.encode err }
showAsServantJSONErr (InternalServerError err)                 = err
showAsServantJSONErr a                                         = err500 { errBody = JSON.encode a }
