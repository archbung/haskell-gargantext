{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE RecordWildCards          #-}

module Gargantext.API.Errors where

import Prelude
import GHC.Stack
import Control.Exception
import qualified Data.Text as T
import Data.Kind
import Data.Singletons.TH
import qualified Network.HTTP.Types as HTTP
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Data.Aeson as JSON
import GHC.Generics

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

-- | A (hopefully and eventually) exhaustive list of backend errors.
data BackendErrorType
  = BE_phylo_corpus_not_ready
  | BE_not_good_enough_ratio
  | BE_node_not_found
  deriving (Show, Eq, Enum, Bounded)

$(genSingletons [''BackendErrorType])

-- | An error that can be returned to the frontend. It carries a human-friendly
-- diagnostic, the 'type' of the error as well as some context-specific data.
data FrontendError where
  FrontendError :: forall b. (IsFrontendErrorData b) =>
    { fe_diagnostic :: !T.Text
    , fe_type       :: !BackendErrorType
    , fe_data       :: ToFrontendErrorData b
    } -> FrontendError

deriving instance Show FrontendError

class (SingI payload, ToJSON (ToFrontendErrorData payload)
                    -- , FromJSON (ToFrontendErrorData payload)
                    , Show (ToFrontendErrorData payload)
                    ) => IsFrontendErrorData payload

instance IsFrontendErrorData 'BE_phylo_corpus_not_ready
instance IsFrontendErrorData 'BE_not_good_enough_ratio
instance IsFrontendErrorData 'BE_node_not_found

data family ToFrontendErrorData (payload :: BackendErrorType) :: Type

data instance ToFrontendErrorData 'BE_phylo_corpus_not_ready =
  PhyloCorpusNotReady deriving (Show, Generic)
data instance ToFrontendErrorData 'BE_not_good_enough_ratio  =
  NotGoodEnoughRatio deriving (Show, Generic)
data instance ToFrontendErrorData 'BE_node_not_found         =
  NodeNotFound deriving (Show, Generic)

instance ToJSON (ToFrontendErrorData 'BE_phylo_corpus_not_ready)
instance ToJSON (ToFrontendErrorData 'BE_not_good_enough_ratio)
instance ToJSON (ToFrontendErrorData 'BE_node_not_found)

mkFrontendErr :: IsFrontendErrorData payload
              => Proxy (payload :: BackendErrorType)
              -> ToFrontendErrorData payload
              -> FrontendError
mkFrontendErr et = mkFrontendErr' mempty et

mkFrontendErr' :: IsFrontendErrorData payload
               => T.Text
               -> Proxy (payload :: BackendErrorType)
               -> ToFrontendErrorData payload
               -> FrontendError
mkFrontendErr' diag (Proxy :: Proxy payload) pl = FrontendError diag (fromSing $ sing @payload) pl

instance Arbitrary BackendErrorType where
  arbitrary = arbitraryBoundedEnum

backendErrorTypeToErrStatus :: BackendErrorType -> HTTP.Status
backendErrorTypeToErrStatus = \case
  BE_phylo_corpus_not_ready -> HTTP.status500
  BE_not_good_enough_ratio  -> HTTP.status500
  BE_node_not_found         -> HTTP.status500

instance Arbitrary FrontendError where
  arbitrary = do
    et  <- arbitrary
    txt <- arbitrary
    genFrontendErr txt et

genFrontendErr :: T.Text -> BackendErrorType -> Gen FrontendError
genFrontendErr txt be = case be of
  BE_phylo_corpus_not_ready
    -> pure $ mkFrontendErr' txt (Proxy @'BE_phylo_corpus_not_ready) PhyloCorpusNotReady
  BE_not_good_enough_ratio
    -> pure $ mkFrontendErr' txt (Proxy @'BE_not_good_enough_ratio) NotGoodEnoughRatio
  BE_node_not_found
    -> pure $ mkFrontendErr' txt (Proxy @'BE_node_not_found) NodeNotFound

-- | This compiles if we use the correct payload type, or otherwise it won't:
-- >>> mkFrontendErr (Proxy @'BE_phylo_corpus_not_ready) NodeNotFound
myTest :: FrontendError
myTest = mkFrontendErr (Proxy @'BE_phylo_corpus_not_ready) PhyloCorpusNotReady

instance ToJSON BackendErrorType where
  toJSON = \case
    BE_phylo_corpus_not_ready -> JSON.String "phylo_corpus_not_ready"
    BE_not_good_enough_ratio  -> JSON.String "not_good_enough_ratio"
    BE_node_not_found         -> JSON.String "node_not_found"

instance ToJSON FrontendError where
  toJSON fe = JSON.object [ "diagnostic" .= toJSON (fe_diagnostic fe)
                          , "type"       .= toJSON (fe_type fe)
                          , "data"       .= case fe of (FrontendError _ _ dt) -> toJSON dt
                          ]
