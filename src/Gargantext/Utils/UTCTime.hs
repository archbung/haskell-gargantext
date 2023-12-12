{-|
Module      : Gargantext.Utils.UTCTime
Description : Gargantext utilities
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Gargantext.Utils.UTCTime where

import Data.Aeson (FromJSON, ToJSON)
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types (GQLType(..), DecodeScalar(..), EncodeScalar(..))
import Data.Morpheus.Types qualified as DMT
import Data.String (fromString)
import Data.Swagger (ToSchema)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Gargantext.Prelude
import Gargantext.System.Logging
import Prelude (String)


newtype NUTCTime = NUTCTime UTCTime
  deriving (Eq, Show, Generic)
instance DecodeScalar NUTCTime where
  decodeScalar (DMT.String x) = case (readEither $ T.unpack x) of
    Right r  -> pure $ NUTCTime r
    Left err -> Left $ T.pack err
  decodeScalar          _ = Left "Invalid value for NUTCTime"
instance EncodeScalar NUTCTime where
  encodeScalar (NUTCTime x) = DMT.String $ T.pack $ show x
instance GQLType NUTCTime where
  type KIND NUTCTime = SCALAR
instance FromJSON NUTCTime
instance ToJSON NUTCTime
instance ToSchema NUTCTime

timeMeasured :: (MonadLogger m, MonadBase IO m, HasCallStack)
             => String
             -- ^ A label
             -> m a
             -- ^ The action to run
             -> m a
timeMeasured = withFrozenCallStack $ timeMeasured' DEBUG

timeMeasured' :: (MonadLogger m, MonadBase IO m, HasCallStack)
             => LogLevel
             -- ^ The severity of the log
             -> String
             -- ^ A label to identify the action.
             -> m a
             -- ^ The action to run
             -> m a
timeMeasured' severity label action = withFrozenCallStack $ do
  startTime <- liftBase getPOSIXTime
  res       <- action
  endTime   <- liftBase getPOSIXTime
  let msg = label <> " took " <> (show $ endTime - startTime) <> " seconds to execute."
  $(logLocM) severity (fromString msg)
  return res
