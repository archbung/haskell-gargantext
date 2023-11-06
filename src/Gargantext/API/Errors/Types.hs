{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans          #-} -- instance IsFrontendErrorData and stage restriction

module Gargantext.API.Errors.Types (
  -- * The main frontend error type
    FrontendError(..)

  -- * The internal backend type and an enumeration of all possible backend error types
  , BackendErrorCode(..)
  , BackendInternalError(..)
  , ToFrontendErrorData(..)

  -- * Constructing frontend errors
  , mkFrontendErrNoDiagnostic
  , mkFrontendErrShow
  , mkFrontendErr'

  -- * Evidence carrying
  , Dict(..)
  , IsFrontendErrorData(..)

  -- * Generating test cases
  , genFrontendErr

  -- * Attaching callstacks to exceptions
  , WithStacktrace(..)
  ) where

import Control.Exception
import Control.Lens (makePrisms)
import Data.Aeson as JSON
import Data.Aeson.Types (typeMismatch, emptyArray)
import Data.Singletons.TH
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable
import Data.Validity (Validation(..), ValidationChain (..), prettyValidation)
import GHC.Generics
import GHC.Stack
import Gargantext.API.Errors.Class
import Gargantext.API.Errors.TH
import Gargantext.API.Errors.Types.Backend
import Gargantext.Core.Types (HasValidationError(..))
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Query.Tree.Error
import Gargantext.Utils.Dict
import Prelude
import Servant (ServerError)
import Servant.Job.Core
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import qualified Data.Text as T
import qualified Gargantext.Utils.Jobs.Monad as Jobs
import qualified Servant.Job.Types as SJ
import Text.Read (readMaybe)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Gargantext.API.Admin.Auth.Types (AuthenticationError)

-- | A 'WithStacktrace' carries an error alongside its
-- 'CallStack', to be able to print the correct source location
-- of where the error originated.
data WithStacktrace e =
  WithStacktrace {
    ct_callStack :: !CallStack
  , ct_error     :: !e
  } deriving Show

instance Exception e => Exception (WithStacktrace e) where
  displayException WithStacktrace{..}
    = displayException ct_error <> "\n" <> prettyCallStack ct_callStack

-------------------------------------------------------------------
-- | An internal error which can be emitted from the backend and later
-- converted into a 'FrontendError', for later consumption.
data BackendInternalError
  = InternalNodeError           !NodeError
  | InternalTreeError           !TreeError
  | InternalValidationError     !Validation
  | InternalAuthenticationError !AuthenticationError
  | InternalServerError         !ServerError
  | InternalJobError            !Jobs.JobError
  deriving (Show, Typeable)

makePrisms ''BackendInternalError

instance ToJSON BackendInternalError where
  toJSON (InternalJobError s) =
    object [ ("status", toJSON SJ.IsFailure)
           , ("log", emptyArray)
           , ("id", String mk_id)
           , ("error", String $ T.pack $ show s) ]
    where
      mk_id = case s of
        Jobs.InvalidMacID i -> i
        _ -> ""
  toJSON err = object [("error", String $ T.pack $ show err)]

instance Exception BackendInternalError

instance HasNodeError BackendInternalError where
  _NodeError = _InternalNodeError

instance HasValidationError BackendInternalError where
  _ValidationError = _InternalValidationError

instance HasTreeError BackendInternalError where
  _TreeError = _InternalTreeError

instance HasServerError BackendInternalError where
  _ServerError = _InternalServerError

instance HasAuthenticationError BackendInternalError where
  _AuthenticationError = _InternalAuthenticationError

-- | An error that can be returned to the frontend. It carries a human-friendly
-- diagnostic, the 'type' of the error as well as some context-specific data.
data FrontendError where
  FrontendError :: forall b. IsFrontendErrorData b =>
    { fe_diagnostic :: !T.Text
    , fe_type       :: !BackendErrorCode
    , fe_data       :: ToFrontendErrorData b
    } -> FrontendError

-- | Creates an error without attaching a diagnostic to it.
mkFrontendErrNoDiagnostic :: IsFrontendErrorData payload
                          => ToFrontendErrorData payload
                          -> FrontendError
mkFrontendErrNoDiagnostic et = mkFrontendErr' mempty et

-- | Renders the error by using as a diagnostic the string
-- resulting from 'Show'ing the underlying type.
mkFrontendErrShow :: IsFrontendErrorData payload
                  => ToFrontendErrorData payload
                  -> FrontendError
mkFrontendErrShow et = mkFrontendErr' (T.pack $ show et) et

mkFrontendErr' :: forall payload. IsFrontendErrorData payload
               => T.Text
               -> ToFrontendErrorData (payload :: BackendErrorCode)
               -> FrontendError
mkFrontendErr' diag pl = FrontendError diag (fromSing $ sing @payload) pl

deriving instance Show FrontendError
instance Eq FrontendError where
  f1 == f2 = case (f1, f2) of
    (FrontendError fe_diagnostic_1 fe_type_1 (fe_data_1 :: ToFrontendErrorData b1),
     FrontendError fe_diagnostic_2 fe_type_2 (fe_data_2 :: ToFrontendErrorData b2))
       -> fe_diagnostic_1 == fe_diagnostic_2 && fe_type_1 == fe_type_2 &&
          case eqT @b1 @b2 of
            Nothing   -> False
            Just Refl -> fe_data_1 == fe_data_2

$(deriveIsFrontendErrorData ''BackendErrorCode)

----------------------------------------------------------------------------
-- ToFrontendErrorData data family instances
----------------------------------------------------------------------------

data NoFrontendErrorData = NoFrontendErrorData

newtype instance ToFrontendErrorData 'EC_404__node_error_list_not_found =
  FE_node_error_list_not_found { lnf_list_id :: ListId }
  deriving (Show, Eq, Generic)

data instance ToFrontendErrorData 'EC_404__node_error_root_not_found =
  FE_node_error_root_not_found
  deriving (Show, Eq, Generic)

data instance ToFrontendErrorData 'EC_404__node_error_corpus_not_found =
  FE_node_error_corpus_not_found
  deriving (Show, Eq, Generic)

data instance ToFrontendErrorData 'EC_500__node_error_not_implemented_yet =
  FE_node_error_not_implemented_yet
  deriving (Show, Eq, Generic)

data instance ToFrontendErrorData 'EC_404__node_error_not_found =
  FE_node_error_not_found { nenf_node_id :: !NodeId }
  deriving (Show, Eq, Generic)

--
-- validation errors
--

data instance ToFrontendErrorData 'EC_400__validation_error =
  FE_validation_error { validation_error :: T.Text }
  deriving (Show, Eq, Generic)

--
-- authentication errors
--

data instance ToFrontendErrorData 'EC_403__login_failed_error =
  FE_login_failed_error { lfe_node_id :: NodeId
                        , lfe_user_id :: UserId
                        }
  deriving (Show, Eq, Generic)


--
-- Tree errors
--

data instance ToFrontendErrorData 'EC_404__tree_error_root_not_found =
  FE_tree_error_root_not_found
  deriving (Show, Eq, Generic)

data instance ToFrontendErrorData 'EC_404__tree_error_empty_root =
  FE_tree_error_empty_root
  deriving (Show, Eq, Generic)

data instance ToFrontendErrorData 'EC_500__tree_error_too_many_roots =
  FE_tree_error_too_many_roots { tmr_roots :: NonEmpty NodeId }
  deriving (Show, Eq, Generic)

--
-- Job errors
--

data instance ToFrontendErrorData 'EC_500__job_error_invalid_id_type =
  FE_job_error_invalid_id_type { jeiit_type :: T.Text }
  deriving (Show, Eq, Generic)

data instance ToFrontendErrorData 'EC_500__job_error_expired =
  FE_job_error_expired { jee_job_id :: Int }
  deriving (Show, Eq, Generic)

data instance ToFrontendErrorData 'EC_500__job_error_invalid_mac =
  FE_job_error_invalid_mac { jeim_mac :: T.Text }
  deriving (Show, Eq, Generic)

data instance ToFrontendErrorData 'EC_500__job_error_unknown_job =
  FE_job_error_unknown_job { jeuj_job_id :: Int }
  deriving (Show, Eq, Generic)

data instance ToFrontendErrorData 'EC_500__internal_server_error =
  FE_internal_server_error { ise_error :: T.Text }
  deriving (Show, Eq, Generic)

data instance ToFrontendErrorData 'EC_500__job_error_generic_exception =
  FE_job_error_generic_exception { jege_error :: T.Text }
  deriving (Show, Eq, Generic)

----------------------------------------------------------------------------
-- JSON instances. It's important to have nice and human readable instances.
-- It's also important that they all roundtrips, i.e. that given a 'ToFrontendErrorData'
-- payload, we can render it to JSON and parse it back.
----------------------------------------------------------------------------

instance ToJSON (ToFrontendErrorData 'EC_404__node_error_list_not_found) where
  toJSON (FE_node_error_list_not_found lid) =
    JSON.object [ "list_id" .= toJSON lid ]

instance FromJSON (ToFrontendErrorData 'EC_404__node_error_list_not_found) where
  parseJSON = withObject "FE_node_error_list_not_found" $ \o -> do
    lnf_list_id <- o .: "list_id"
    pure FE_node_error_list_not_found{..}

instance ToJSON (ToFrontendErrorData 'EC_404__node_error_root_not_found) where
  toJSON _ = JSON.Null

instance FromJSON (ToFrontendErrorData 'EC_404__node_error_root_not_found) where
  parseJSON _ = pure FE_node_error_root_not_found

instance ToJSON (ToFrontendErrorData 'EC_404__node_error_corpus_not_found) where
  toJSON _ = JSON.Null

instance FromJSON (ToFrontendErrorData 'EC_404__node_error_corpus_not_found) where
  parseJSON _ = pure FE_node_error_corpus_not_found

instance ToJSON (ToFrontendErrorData 'EC_500__node_error_not_implemented_yet) where
  toJSON _ = JSON.Null

instance FromJSON (ToFrontendErrorData 'EC_500__node_error_not_implemented_yet) where
  parseJSON _ = pure FE_node_error_not_implemented_yet

instance ToJSON (ToFrontendErrorData 'EC_404__node_error_not_found) where
  toJSON (FE_node_error_not_found nodeId) = object [ "node_id" .= toJSON nodeId ]

instance FromJSON (ToFrontendErrorData 'EC_404__node_error_not_found) where
  parseJSON = withObject "FE_node_error_not_found" $ \o -> do
    nenf_node_id <- o .: "node_id"
    pure FE_node_error_not_found{..}

--
-- validation errors
--

instance ToJSON (ToFrontendErrorData 'EC_400__validation_error) where
  toJSON (FE_validation_error val) = toJSON val

instance FromJSON (ToFrontendErrorData 'EC_400__validation_error) where
  parseJSON (String txt) = pure $ FE_validation_error txt
  parseJSON ty           = typeMismatch "FE_validation_error" ty

--
-- authentication errors
--

instance ToJSON (ToFrontendErrorData 'EC_403__login_failed_error) where
  toJSON FE_login_failed_error{..} =
    object [ "user_id" .= toJSON lfe_user_id, "node_id" .= toJSON lfe_node_id ]

instance FromJSON (ToFrontendErrorData 'EC_403__login_failed_error) where
  parseJSON = withObject "FE_login_failed_error" $ \o -> do
    lfe_user_id <- o .: "user_id"
    lfe_node_id <- o .: "node_id"
    pure FE_login_failed_error{..}

--
-- internal server errors
--

instance ToJSON (ToFrontendErrorData 'EC_500__internal_server_error) where
  toJSON FE_internal_server_error{..} = object [ "error" .= toJSON ise_error ]

instance FromJSON (ToFrontendErrorData 'EC_500__internal_server_error) where
  parseJSON = withObject "FE_internal_server_error" $ \o -> do
    ise_error <- o .: "error"
    pure FE_internal_server_error{..}


--
-- tree errors
--

instance ToJSON (ToFrontendErrorData 'EC_404__tree_error_root_not_found) where
  toJSON _ = JSON.Null

instance FromJSON (ToFrontendErrorData 'EC_404__tree_error_root_not_found) where
  parseJSON _ = pure FE_tree_error_root_not_found

instance ToJSON (ToFrontendErrorData 'EC_404__tree_error_empty_root) where
  toJSON _ = JSON.Null

instance FromJSON (ToFrontendErrorData 'EC_404__tree_error_empty_root) where
  parseJSON _ = pure FE_tree_error_empty_root

instance ToJSON (ToFrontendErrorData 'EC_500__tree_error_too_many_roots) where
  toJSON (FE_tree_error_too_many_roots roots) =
    object [ "node_ids" .= NE.toList roots ]

instance FromJSON (ToFrontendErrorData 'EC_500__tree_error_too_many_roots) where
  parseJSON = withObject "FE_tree_error_too_many_roots" $ \o -> do
    tmr_roots <- o .: "node_ids"
    pure FE_tree_error_too_many_roots{..}

--
-- job errors
--

instance ToJSON (ToFrontendErrorData 'EC_500__job_error_invalid_id_type) where
  toJSON (FE_job_error_invalid_id_type idTy) =
    object [ "type" .= toJSON idTy ]

instance FromJSON (ToFrontendErrorData 'EC_500__job_error_invalid_id_type) where
  parseJSON = withObject "FE_job_error_invalid_id_type" $ \o -> do
    jeiit_type <- o .: "type"
    pure FE_job_error_invalid_id_type{..}

instance ToJSON (ToFrontendErrorData 'EC_500__job_error_expired) where
  toJSON (FE_job_error_expired jobId) =
    object [ "job_id" .= toJSON jobId ]

instance FromJSON (ToFrontendErrorData 'EC_500__job_error_expired) where
  parseJSON = withObject "FE_job_error_expired" $ \o -> do
    jee_job_id <- o .: "job_id"
    pure FE_job_error_expired{..}

instance ToJSON (ToFrontendErrorData 'EC_500__job_error_invalid_mac) where
  toJSON (FE_job_error_invalid_mac mac) =
    object [ "mac" .= toJSON mac ]

instance FromJSON (ToFrontendErrorData 'EC_500__job_error_invalid_mac) where
  parseJSON = withObject "FE_job_error_invalid_mac" $ \o -> do
    jeim_mac <- o .: "mac"
    pure FE_job_error_invalid_mac{..}

instance ToJSON (ToFrontendErrorData 'EC_500__job_error_unknown_job) where
  toJSON (FE_job_error_unknown_job jobId) =
    object [ "job_id" .= toJSON jobId ]

instance FromJSON (ToFrontendErrorData 'EC_500__job_error_unknown_job) where
  parseJSON = withObject "FE_job_error_unknown_job" $ \o -> do
    jeuj_job_id <- o .: "job_id"
    pure FE_job_error_unknown_job{..}

instance ToJSON (ToFrontendErrorData 'EC_500__job_error_generic_exception) where
  toJSON (FE_job_error_generic_exception err) =
    object [ "error" .= toJSON err ]

instance FromJSON (ToFrontendErrorData 'EC_500__job_error_generic_exception) where
  parseJSON = withObject "FE_job_error_generic_exception" $ \o -> do
    jege_error <- o .: "error"
    pure FE_job_error_generic_exception{..}

----------------------------------------------------------------------------
-- Arbitrary instances and test data generation
----------------------------------------------------------------------------

instance Arbitrary BackendErrorCode where
  arbitrary = arbitraryBoundedEnum

genFrontendErr :: BackendErrorCode -> Gen FrontendError
genFrontendErr be = do
  txt <- arbitrary
  case be of

    -- node errors
    EC_404__node_error_list_not_found
      -> arbitrary >>= \lid -> pure $ mkFrontendErr' txt $ FE_node_error_list_not_found lid
    EC_404__node_error_root_not_found
      -> pure $ mkFrontendErr' txt FE_node_error_root_not_found
    EC_404__node_error_corpus_not_found
      -> pure $ mkFrontendErr' txt FE_node_error_corpus_not_found
    EC_500__node_error_not_implemented_yet
      -> pure $ mkFrontendErr' txt FE_node_error_not_implemented_yet
    EC_404__node_error_not_found
      -> do nodeId <- arbitrary
            pure $ mkFrontendErr' txt (FE_node_error_not_found nodeId)

    -- validation error
    EC_400__validation_error
      -> do let genValChain = oneof [ Violated <$> arbitrary, Location <$> arbitrary <*> genValChain ]
            chain <- listOf1 genValChain
            pure $ mkFrontendErr' txt $ FE_validation_error (T.pack $ fromMaybe "unknown_validation_error" $ prettyValidation $ Validation chain)

    -- authentication error
    EC_403__login_failed_error
      -> do nid <- arbitrary
            uid <- arbitrary
            pure $ mkFrontendErr' txt $ FE_login_failed_error nid uid

    -- internal error
    EC_500__internal_server_error
      -> do err <- arbitrary
            pure $ mkFrontendErr' txt $ FE_internal_server_error err

    -- tree errors
    EC_404__tree_error_root_not_found
      -> pure $ mkFrontendErr' txt $ FE_tree_error_root_not_found
    EC_404__tree_error_empty_root
      -> pure $ mkFrontendErr' txt $ FE_tree_error_empty_root
    EC_500__tree_error_too_many_roots
      -> do nodes <- arbitrary
            pure $ mkFrontendErr' txt $ FE_tree_error_too_many_roots nodes

    -- job errors
    EC_500__job_error_invalid_id_type
      -> do idTy <- arbitrary
            pure $ mkFrontendErr' txt $ FE_job_error_invalid_id_type idTy
    EC_500__job_error_expired
      -> do jobId <- getPositive <$> arbitrary
            pure $ mkFrontendErr' txt $ FE_job_error_expired jobId
    EC_500__job_error_invalid_mac
      -> do macId <- arbitrary
            pure $ mkFrontendErr' txt $ FE_job_error_expired macId
    EC_500__job_error_unknown_job
      -> do jobId <- getPositive <$> arbitrary
            pure $ mkFrontendErr' txt $ FE_job_error_unknown_job jobId
    EC_500__job_error_generic_exception
      -> do err <- arbitrary
            pure $ mkFrontendErr' txt $ FE_job_error_generic_exception err

instance ToJSON BackendErrorCode where
  toJSON = JSON.String . T.pack . drop 3 . show

instance FromJSON BackendErrorCode where
  parseJSON (String s) = case readMaybe (T.unpack $ "EC_" <> s) of
    Just v     -> pure v
    Nothing    -> fail $ "FromJSON BackendErrorCode unexpected value: " <> T.unpack s
  parseJSON ty = typeMismatch "BackendErrorCode" ty

instance ToJSON FrontendError where
  toJSON (FrontendError diag ty dt) =
    JSON.object [ "diagnostic" .= toJSON diag
                , "type"       .= toJSON ty
                , "data"       .= toJSON dt
                ]

instance FromJSON FrontendError where
  parseJSON = withObject "FrontendError" $ \o -> do
    (fe_diagnostic :: T.Text)      <- o .: "diagnostic"
    (fe_type :: BackendErrorCode)  <- o .: "type"
    case fe_type of
      EC_404__node_error_list_not_found         -> do
        (fe_data :: ToFrontendErrorData 'EC_404__node_error_list_not_found) <- o .: "data"
        pure FrontendError{..}
      EC_404__node_error_root_not_found         -> do
        (fe_data :: ToFrontendErrorData 'EC_404__node_error_root_not_found) <- o .: "data"
        pure FrontendError{..}
      EC_404__node_error_corpus_not_found       -> do
        (fe_data :: ToFrontendErrorData 'EC_404__node_error_corpus_not_found) <- o .: "data"
        pure FrontendError{..}
      EC_404__node_error_not_found -> do
        (fe_data :: ToFrontendErrorData 'EC_404__node_error_not_found) <- o .: "data"
        pure FrontendError{..}
      EC_500__node_error_not_implemented_yet -> do
        (fe_data :: ToFrontendErrorData 'EC_500__node_error_not_implemented_yet) <- o .: "data"
        pure FrontendError{..}

      -- validation error
      EC_400__validation_error -> do
        (fe_data :: ToFrontendErrorData 'EC_400__validation_error) <- o .: "data"
        pure FrontendError{..}

      -- authentication errors
      EC_403__login_failed_error -> do
        (fe_data :: ToFrontendErrorData 'EC_403__login_failed_error) <- o .: "data"
        pure FrontendError{..}

      -- internal server error
      EC_500__internal_server_error -> do
        (fe_data :: ToFrontendErrorData 'EC_500__internal_server_error) <- o .: "data"
        pure FrontendError{..}

      -- tree errors
      EC_404__tree_error_root_not_found -> do
        (fe_data :: ToFrontendErrorData 'EC_404__tree_error_root_not_found) <- o .: "data"
        pure FrontendError{..}
      EC_404__tree_error_empty_root -> do
        (fe_data :: ToFrontendErrorData 'EC_404__tree_error_empty_root) <- o .: "data"
        pure FrontendError{..}
      EC_500__tree_error_too_many_roots -> do
        (fe_data :: ToFrontendErrorData 'EC_500__tree_error_too_many_roots) <- o .: "data"
        pure FrontendError{..}

      -- job errors
      EC_500__job_error_invalid_id_type -> do
        (fe_data :: ToFrontendErrorData 'EC_500__job_error_invalid_id_type) <- o .: "data"
        pure FrontendError{..}
      EC_500__job_error_expired -> do
        (fe_data :: ToFrontendErrorData 'EC_500__job_error_expired) <- o .: "data"
        pure FrontendError{..}
      EC_500__job_error_invalid_mac -> do
        (fe_data :: ToFrontendErrorData 'EC_500__job_error_invalid_mac) <- o .: "data"
        pure FrontendError{..}
      EC_500__job_error_unknown_job -> do
        (fe_data :: ToFrontendErrorData 'EC_500__job_error_unknown_job) <- o .: "data"
        pure FrontendError{..}
      EC_500__job_error_generic_exception -> do
        (fe_data :: ToFrontendErrorData 'EC_500__job_error_generic_exception) <- o .: "data"
        pure FrontendError{..}
