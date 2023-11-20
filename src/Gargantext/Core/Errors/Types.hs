module Gargantext.Core.Errors.Types (
  -- * Attaching callstacks to exceptions
    WithStacktrace(..)
  , withStacktrace
  ) where

import Control.Exception
import GHC.Stack
import Prelude

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

withStacktrace :: HasCallStack => e -> WithStacktrace e
withStacktrace = withFrozenCallStack . WithStacktrace callStack
