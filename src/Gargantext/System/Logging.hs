{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Gargantext.System.Logging (
    LogLevel(..)
  , HasLogger(..)
  , MonadLogger(..)
  , logM
  , logLocM
  , withLogger
  , withLoggerHoisted
  ) where

import Language.Haskell.TH hiding (Type)
import Control.Exception.Lifted (bracket)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Kind (Type)
import Prelude
import qualified Data.Text as T
import qualified Language.Haskell.TH.Syntax        as TH

data LogLevel =
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
  data family Logger m        :: Type
  type family LogInitParams m :: Type
  type family LogPayload m    :: Type
  initLogger    :: LogInitParams m -> (forall m1. MonadIO m1 => m1 (Logger m))
  destroyLogger :: Logger m        -> (forall m1. MonadIO m1 => m1 ())
  logMsg        :: Logger m        -> LogLevel -> LogPayload m -> m ()
  logTxt        :: Logger m        -> LogLevel -> T.Text -> m ()

-- | Separate typeclass to get hold of a 'Logger' from within a monad.
-- We keey 'HasLogger' and 'MonadLogger' separate to enforce compositionality,
-- i.e. we can still give instances to 'HasLogger' for things like 'IO' without
-- having to force actually acquiring a logger for those monads.
class HasLogger m => MonadLogger m where
  getLogger :: m (Logger m)

-- | A variant of 'logTxt' that doesn't require passing an explicit 'Logger'.
logM :: (Monad m, MonadLogger m) => LogLevel -> T.Text -> m ()
logM level msg = do
  logger <- getLogger
  logTxt logger level msg

-- | Like 'logM', but it automatically adds the file and line number to
-- the output log.
logLocM :: ExpQ
logLocM = [| \level msg ->
  let loc = $(getLocTH)
  in logM level (formatWithLoc loc msg)
  |]

formatWithLoc :: Loc -> T.Text -> T.Text
formatWithLoc loc msg = "[" <> locationToText <> "] " <> msg
  where
    locationToText :: T.Text
    locationToText = T.pack $ (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
      where
        line = show . fst . loc_start
        char = show . snd . loc_start

getLocTH :: ExpQ
getLocTH = [| $(location >>= liftLoc) |]

liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(TH.lift a)
    $(TH.lift b)
    $(TH.lift c)
    ($(TH.lift d1), $(TH.lift d2))
    ($(TH.lift e1), $(TH.lift e2))
    |]

-- | exception-safe combinator that creates and destroys a logger.
-- Think about it like a 'bracket' function from 'Control.Exception'.
withLogger :: (MonadBaseControl IO m, MonadIO m, HasLogger m)
           => LogInitParams m
           -> (Logger m -> m a)
           -> m a
withLogger params = bracket (initLogger params) destroyLogger

-- | Like 'withLogger', but it allows creating a 'Logger' that can run in
-- a different monad from within an 'IO' action.
withLoggerHoisted :: (MonadBaseControl IO m, HasLogger m)
                  => LogInitParams m
                  -> (Logger m -> IO a)
                  -> IO a
withLoggerHoisted params act = bracket (initLogger params) destroyLogger act
