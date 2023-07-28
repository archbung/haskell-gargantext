{-# LANGUAGE TypeFamilies #-}

module Gargantext.System.Logging where

import Prelude
import Data.Kind (Type)
import Control.Monad.Trans.Control
import Control.Exception.Lifted (bracket)

data Level =
  -- | Debug messages
  DEBUG
  -- | Information
  | INFO
  -- | Normal runtime conditions
  | NOTICE
  -- | General Warnings
  | WARNING
  -- | General Errors
  | ERROR
  -- | Severe situations
  | CRITICAL
  -- | Take immediate action
  | ALERT
  -- | System is unusable
  | EMERGENCY
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | This is a barebore logging interface which we
-- can extend to plug a proper logging library, without
-- the details of the logger cropping up everywhere in
-- the rest of the codebase.
class HasLogger m where
  data family Logger m     :: Type
  type family InitParams m :: Type
  type family Payload m    :: Type
  initLogger    :: InitParams m -> m (Logger m)
  destroyLogger :: Logger m -> m ()
  logMsg :: Logger m -> Level -> Payload m -> m ()

-- | exception-safe combinator that creates and destroys a logger.
-- Think about it like a 'bracket' function from 'Control.Exception'.
withLogger :: (MonadBaseControl IO m, HasLogger m)
           => InitParams m
           -> (Logger m -> m a)
           -> m a
withLogger params = bracket (initLogger params) destroyLogger
