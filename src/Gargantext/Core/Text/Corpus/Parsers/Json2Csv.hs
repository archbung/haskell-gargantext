{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.Json2Csv
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Json parser to export towoard CSV GargV3 format.
(Export from the Patent Database.)

-}

{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Core.Text.Corpus.Parsers.Json2Csv (json2csv, readPatents)
  where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Lazy (readFile)
import Data.Text (unpack)
import Data.Vector (fromList)
import Gargantext.Core.Text.Corpus.Parsers.CSV (CsvDoc(..), writeFile, headerCsvGargV3)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Defaults qualified as Defaults
import Gargantext.Prelude hiding (readFile, writeFile)
import Prelude (read)

data Patent = Patent { _patent_title    :: Text
                     , _patent_abstract :: Text
                     , _patent_year     :: Text
                     , _patent_id       :: Text
 } deriving (Show)

$(deriveJSON (unPrefix "_patent_") ''Patent)

readPatents :: FilePath -> IO (Maybe [Patent])
readPatents fp = decode <$> readFile fp

type FilePathIn  = FilePath
type FilePathOut = FilePath

json2csv :: FilePathIn -> FilePathOut -> IO ()
json2csv fin fout = do
  patents <- maybe (panicTrace "json2csv error") identity <$> readPatents fin
  writeFile fout (headerCsvGargV3, fromList $ map patent2csvDoc patents)

patent2csvDoc :: Patent -> CsvDoc
patent2csvDoc (Patent { .. }) =
  CsvDoc { csv_title = _patent_title
         , csv_source = "Source"
         , csv_publication_year = Just $ read (unpack _patent_year)
         , csv_publication_month = Just $ Defaults.month
         , csv_publication_day = Just $ Defaults.day
         , csv_abstract = _patent_abstract
         , csv_authors = "Authors" }





