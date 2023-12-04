{-|
Module      : Main.hs
Description : Gargantext starter
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Script to start gargantext with different modes (Dev, Prod, Mock).

-}

{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}


module Main where


import Data.Text (unpack)
import Data.Version (showVersion)
import Gargantext.API (startGargantext) -- , startGargantextMock)
import Gargantext.API.Admin.EnvTypes
import Gargantext.Prelude
import Gargantext.System.Logging
import GHC.IO.Encoding
import Options.Generic
import System.Exit (exitSuccess)
import qualified Paths_gargantext as PG -- cabal magic build module


instance ParseRecord Mode
instance ParseField  Mode
instance ParseFields Mode

data MyOptions w =
  MyOptions { run  :: w ::: Mode
                        <?> "Possible modes: Dev | Mock | Prod"
            , port :: w ::: Maybe Int
                        <?> "By default: 8008"
            , ini  :: w ::: Maybe Text
                        <?> "Ini-file path of gargantext.ini"
            , version :: w ::: Bool
                        <?> "Show version number and exit"
            }
   deriving (Generic)

instance ParseRecord (MyOptions Wrapped)
deriving instance Show (MyOptions Unwrapped)

main :: IO ()
main = withLogger () $ \ioLogger -> do

  -- Sets the locale to avoid encoding issues like in #284.
  setLocaleEncoding utf8
  currentLocale <- getLocaleEncoding

  MyOptions myMode myPort myIniFile myVersion  <- unwrapRecord
          "Gargantext server"
  ---------------------------------------------------------------
  if myVersion then do
    logMsg ioLogger INFO $ "Version: " <> showVersion PG.version
    System.Exit.exitSuccess
  else
    return ()
  ---------------------------------------------------------------
  let myPort' = case myPort of
        Just p  -> p
        Nothing -> 8008

      myIniFile' = case myIniFile of
          Nothing -> panicTrace "[ERROR] gargantext.ini needed"
          Just i  -> i

  ---------------------------------------------------------------
  let start = case myMode of
        Mock -> panicTrace "[ERROR] Mock mode unsupported"
        _ -> startGargantext myMode myPort' (unpack myIniFile')
  logMsg ioLogger INFO $ "Starting with "   <> show myMode <> " mode."
  logMsg ioLogger INFO $ "Machine locale: " <> show currentLocale
  start
  ---------------------------------------------------------------
