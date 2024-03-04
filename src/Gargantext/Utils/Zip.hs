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


import "zip" Codec.Archive.Zip (addEntry, createArchive, mkEntrySelector, withArchive, CompressionMethod(BZip2), ZipArchive)
import "zip-archive" Codec.Archive.Zip qualified as ZArch
import Control.Monad.Base (MonadBase, liftBase)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSC
import Protolude
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)


-- | Take a zip file (in for of a ByteString) and work on its contents (using the ZipArchive monad)
withZipFileBS :: MonadIO m => BS.ByteString -> ZipArchive a -> m a
withZipFileBS bs actions = liftIO $
 bracket (emptySystemTempFile "parsed-zip")
         removeFile
         (\path -> do
             BS.writeFile path bs
             withArchive path actions)


-- | Zip ByteString contents and return the ZIP file as ByteString
zipContents :: MonadBase IO m => FilePath -> BS.ByteString -> m BS.ByteString
zipContents fpath bsContents = liftBase $
  bracket (emptySystemTempFile "zip-contents")
          removeFile
          (\path -> do
              s <- mkEntrySelector fpath
              createArchive path (addEntry BZip2 bsContents s)
              BS.readFile path)

-- | Same as zipContents above, but pure (in-memory)
zipContentsPure :: FilePath -> BSC.ByteString -> BSC.ByteString
zipContentsPure fpath bscContents = ZArch.fromArchive (ZArch.addEntryToArchive e ZArch.emptyArchive)
  where
    e = ZArch.toEntry fpath 0 bscContents
