{-# LANGUAGE TemplateHaskell     #-}

module Gargantext.API.Admin.Types where

import Control.Lens
import Control.Monad.Logger (LogLevel)
import GHC.Enum
import Gargantext.API.Admin.Settings.CORS
import Gargantext.Prelude
import Servant.Auth.Server (JWTSettings, CookieSettings(..))
import Servant.Client (BaseUrl)


type PortNumber = Int

data SendEmailType = SendEmailViaAws
                   | LogEmailToConsole
                   | WriteEmailToFile
    deriving (Show, Read, Enum, Bounded, Generic)

data Settings = Settings
    { _corsSettings    :: !CORSSettings   -- CORS settings
    , _appPort         :: !PortNumber
    , _logLevelLimit   :: !LogLevel -- log level from the monad-logger package
--    , _dbServer        :: Text
--    ^ this is not used yet
    , _jwtSettings     :: !JWTSettings
    , _cookieSettings  :: !CookieSettings
    , _sendLoginEmails :: !SendEmailType
    , _scrapydUrl      :: !BaseUrl
    }

makeLenses ''Settings

class HasSettings env where
  settings :: Getter env Settings

instance HasSettings Settings where
  settings = identity

data FireWall = FireWall { unFireWall :: Bool }
