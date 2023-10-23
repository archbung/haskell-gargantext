
module Gargantext.API.Errors where

import Prelude
import GHC.Stack
import Control.Exception
import qualified Data.Text as T

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

-- | An error that can be returned to the frontend. It carries a human-friendly
-- diagnostic, the 'type' of the error as well as some context-specific data.
data FrontendError a
  = FrontendError
    { fe_diagnostic :: !T.Text
    , fe_type       :: !BackendErrorType
    , fe_data       :: Maybe a
    } deriving (Show, Eq)

-- | A (hopefully and eventually) exhaustive list of backend errors.
data BackendErrorType
  = BE_phylo_corpus_not_ready
  | BE_not_good_enough_ratio
  | BE_node_not_found
  deriving (Show, Eq)
