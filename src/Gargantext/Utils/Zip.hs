{-|
Module      : Gargantext.Utils.Zip
Description : Gargantext utilities
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Utilities for handling zip files
-}

{-# LANGUAGE PackageImports    #-}

module Gargantext.Utils.Zip where


import "zip" Codec.Archive.Zip (withArchive, ZipArchive)
-- import Control.Monad.Base (liftBase)
import Data.ByteString qualified as BS
import Protolude
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)


withZipFileBS :: MonadIO m => BS.ByteString -> ZipArchive a -> m a
withZipFileBS bs actions =
  liftIO $ bracket (emptySystemTempFile "parsed-zip")
          (\path -> removeFile path) $
    \path -> do
      BS.writeFile path bs
      withArchive path actions
