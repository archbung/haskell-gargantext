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

module Gargantext.API.Errors.Types (
  -- * The main frontend error type
  FrontendError(..)

  -- * The internal backend type and an enumeration of all possible backend error types
  , BackendErrorCode(..)
  , BackendInternalError(..)
  , ToFrontendErrorData(..)

  -- * Constructing frontend errors
  , mkFrontendErr
  , mkFrontendErr'

  -- * Evidence carrying
  , Dict(..)
  , IsFrontendErrorData(..)

  -- * Attaching callstacks to exceptions
  , WithStacktrace(..)
  ) where

import Control.Exception
import Control.Lens (makePrisms)
import Data.Aeson as JSON
import Data.Aeson.Types (typeMismatch, emptyArray)
import Data.Kind
import Data.Singletons.TH
import Data.Typeable
import Data.Validity (Validation)
import GHC.Generics
import GHC.Stack
import Gargantext.API.Errors.Class
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
import qualified Crypto.JWT as Jose
import qualified Data.Text as T
import qualified Gargantext.Utils.Jobs.Monad as Jobs
import qualified Servant.Job.Types as SJ
import Text.Read (readMaybe)

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
  = InternalNodeError       !NodeError
  | InternalTreeError       !TreeError
  | InternalValidationError !Validation
  | InternalJoseError       !Jose.Error
  | InternalServerError     !ServerError
  | InternalJobError        !Jobs.JobError
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

instance HasJoseError BackendInternalError where
  _JoseError = _InternalJoseError

-- | A (hopefully and eventually) exhaustive list of backend errors.
data BackendErrorCode
  =
  -- node errors
    EC_404__node_error_root_not_found
  | EC_404__node_error_corpus_not_found
  -- tree errors
  | EC_404__tree_error_root_not_found
  deriving (Show, Read, Eq, Enum, Bounded)

$(genSingletons [''BackendErrorCode])

-- | An error that can be returned to the frontend. It carries a human-friendly
-- diagnostic, the 'type' of the error as well as some context-specific data.
data FrontendError where
  FrontendError :: forall b. IsFrontendErrorData b =>
    { fe_diagnostic :: !T.Text
    , fe_type       :: !BackendErrorCode
    , fe_data       :: ToFrontendErrorData b
    } -> FrontendError

deriving instance Show FrontendError
instance Eq FrontendError where
  f1 == f2 = case (f1, f2) of
    (FrontendError fe_diagnostic_1 fe_type_1 (fe_data_1 :: ToFrontendErrorData b1),
     FrontendError fe_diagnostic_2 fe_type_2 (fe_data_2 :: ToFrontendErrorData b2))
       -> fe_diagnostic_1 == fe_diagnostic_2 && fe_type_1 == fe_type_2 &&
          case eqT @b1 @b2 of
            Nothing   -> False
            Just Refl -> fe_data_1 == fe_data_2

class ( SingI payload
      , ToJSON (ToFrontendErrorData payload)
      , FromJSON (ToFrontendErrorData payload)
      , Show (ToFrontendErrorData payload)
      , Eq (ToFrontendErrorData payload)
      , Typeable payload
      ) => IsFrontendErrorData payload where
  isFrontendErrorData :: Proxy payload -> Dict IsFrontendErrorData payload

instance IsFrontendErrorData 'EC_404__node_error_root_not_found where
  isFrontendErrorData _ = Dict
instance IsFrontendErrorData 'EC_404__node_error_corpus_not_found where
  isFrontendErrorData _ = Dict
instance IsFrontendErrorData 'EC_404__tree_error_root_not_found where
  isFrontendErrorData _ = Dict

----------------------------------------------------------------------------
-- This data family maps a 'BackendErrorCode' into a concrete payload.
----------------------------------------------------------------------------

data NoFrontendErrorData = NoFrontendErrorData

data family ToFrontendErrorData (payload :: BackendErrorCode) :: Type

data instance ToFrontendErrorData 'EC_404__node_error_root_not_found =
  FE_node_error_root_not_found
  deriving (Show, Eq, Generic)

data instance ToFrontendErrorData 'EC_404__node_error_corpus_not_found =
  FE_node_error_corpus_not_found
  deriving (Show, Eq, Generic)

data instance ToFrontendErrorData 'EC_404__tree_error_root_not_found =
  RootNotFound { _rnf_rootId :: RootId }
  deriving (Show, Eq, Generic)

----------------------------------------------------------------------------
-- JSON instances. It's important to have nice and human readable instances.
----------------------------------------------------------------------------

instance ToJSON (ToFrontendErrorData 'EC_404__node_error_root_not_found) where
  toJSON _ = JSON.Null

instance FromJSON (ToFrontendErrorData 'EC_404__node_error_root_not_found) where
  parseJSON _ = pure FE_node_error_root_not_found

instance ToJSON (ToFrontendErrorData 'EC_404__node_error_corpus_not_found) where
  toJSON _ = JSON.Null

instance FromJSON (ToFrontendErrorData 'EC_404__node_error_corpus_not_found) where
  parseJSON _ = pure FE_node_error_corpus_not_found

instance ToJSON (ToFrontendErrorData 'EC_404__tree_error_root_not_found) where
  toJSON RootNotFound{..} = object [ "root_id" .= toJSON _rnf_rootId ]

instance FromJSON (ToFrontendErrorData 'EC_404__tree_error_root_not_found) where
  parseJSON = withObject "RootNotFound" $ \o -> do
    _rnf_rootId <- o .: "root_id"
    pure RootNotFound{..}

mkFrontendErr :: IsFrontendErrorData payload
              => ToFrontendErrorData payload
              -> FrontendError
mkFrontendErr et = mkFrontendErr' mempty et

mkFrontendErr' :: forall payload. IsFrontendErrorData payload
               => T.Text
               -> ToFrontendErrorData (payload :: BackendErrorCode)
               -> FrontendError
mkFrontendErr' diag pl = FrontendError diag (fromSing $ sing @payload) pl

instance Arbitrary BackendErrorCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FrontendError where
  arbitrary = do
    et  <- arbitrary
    txt <- arbitrary
    genFrontendErr txt et

genFrontendErr :: T.Text -> BackendErrorCode -> Gen FrontendError
genFrontendErr txt be = case be of
  EC_404__node_error_root_not_found
    -> pure $ mkFrontendErr' txt FE_node_error_root_not_found
  EC_404__node_error_corpus_not_found
    -> pure $ mkFrontendErr' txt FE_node_error_corpus_not_found
  EC_404__tree_error_root_not_found
    -> do rootId <- arbitrary
          pure $ mkFrontendErr' txt (RootNotFound rootId)

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
      EC_404__node_error_root_not_found         -> do
        (fe_data :: ToFrontendErrorData 'EC_404__node_error_root_not_found) <- o .: "data"
        pure FrontendError{..}
      EC_404__node_error_corpus_not_found       -> do
        (fe_data :: ToFrontendErrorData 'EC_404__node_error_corpus_not_found) <- o .: "data"
        pure FrontendError{..}
      EC_404__tree_error_root_not_found -> do
        (fe_data :: ToFrontendErrorData 'EC_404__tree_error_root_not_found) <- o .: "data"
        pure FrontendError{..}
