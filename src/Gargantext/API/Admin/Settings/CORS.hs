{--| Support in Gargantext for CORS (Cross-origin resource sharing) --}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gargantext.API.Admin.Settings.CORS where

import Prelude

import Data.Text qualified as T
import Toml
import Gargantext.System.Logging
import Paths_gargantext
import Data.String
import Control.Arrow
import Control.Lens.TH

newtype CORSOrigin = CORSOrigin { _CORSOrigin :: T.Text }
  deriving (Show, Eq, IsString)

data CORSSettings =
  CORSSettings {
    _corsAllowedOrigins :: [CORSOrigin]
  , _corsAllowedHosts   :: [CORSOrigin]
  -- | If 'True', we will reuse the origin whitelist
  -- as the allowed hosts as well. This allows, for example,
  -- to connect from \"demo.gargantext.org\" to \"dev.sub.gargantext.org\"
  -- and vice-versa.
  , _corsUseOriginsForHosts :: !Bool
  } deriving (Show, Eq)

corsOriginCodec :: TomlBiMap CORSOrigin AnyValue
corsOriginCodec = _Orig >>> _Text
  where
   _Orig :: BiMap e CORSOrigin T.Text
   _Orig = iso _CORSOrigin CORSOrigin

corsSettingsCodec :: TomlCodec CORSSettings
corsSettingsCodec = CORSSettings <$> (Toml.arrayOf corsOriginCodec "allowed-origins" .= _corsAllowedOrigins)
                                 <*> pure mempty -- FIXME(adn) Currently we don't need to support this field.
                                 <*> Toml.bool "use-origins-for-hosts" .= _corsUseOriginsForHosts

-- | Loads the 'CORSSettings' from the 'toml' file.
loadGargCorsSettings :: IO CORSSettings
loadGargCorsSettings = do
  corsFile <- getDataFileName "gargantext-cors-settings.toml"
  tomlRes <- Toml.decodeFileEither corsSettingsCodec corsFile
  case tomlRes of
      Left errs      -> do
        withLogger () $ \ioLogger -> do
          logMsg ioLogger WARNING $ T.unpack $ "Error, gargantext-cors-settings.toml parsing failed: " <> Toml.prettyTomlDecodeErrors errs
          pure $ CORSSettings ["http://localhost:8008"] ["http://localhost:3000"] False
      Right settings0 -> case _corsUseOriginsForHosts settings0 of
        True  -> pure $ settings0 { _corsAllowedHosts = "http://localhost:3000" : (_corsAllowedOrigins settings0) }
        False -> pure $ settings0 { _corsAllowedHosts = "http://localhost:3000" : (_corsAllowedHosts settings0) }


makeLenses ''CORSSettings
