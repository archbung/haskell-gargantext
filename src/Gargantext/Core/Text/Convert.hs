{-|
Module      : Gargantext.Core.Text.Convert
Description : All parsers of Gargantext in one file.
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Format Converter.

-}

{-# LANGUAGE PackageImports    #-}

module Gargantext.Core.Text.Convert (risPress2csvWrite)
    where

import Gargantext.Core.Text.Corpus.Parsers (parseFile, FileFormat(..), FileType(..))
import Gargantext.Core.Text.Corpus.Parsers.CSV (writeDocs2Csv)
import Gargantext.Prelude


risPress2csvWrite :: FilePath -> IO ()
risPress2csvWrite f = do
  eContents <- parseFile RisPresse Plain (f <> ".ris")
  case eContents of
    Right contents -> writeDocs2Csv (f <> ".csv") contents
    Left e         -> panicTrace $ "Error: " <> e



