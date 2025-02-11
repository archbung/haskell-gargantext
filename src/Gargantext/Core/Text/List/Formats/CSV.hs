{-|
Module      : Gargantext.Core.Text.List.Formats.CSV
Description : 
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

CSV parser for Gargantext corpus files.

-}


module Gargantext.Core.Text.List.Formats.CSV where

import Control.Applicative
import Data.ByteString.Lazy qualified as BL
import Data.Csv
import Data.Text (pack)
import Data.Text qualified as DT
import Data.Vector (Vector)
import Data.Vector qualified as V
import Gargantext.Core.Text.Context
import Gargantext.Prelude hiding (length)

------------------------------------------------------------------------
csvMapTermList :: FilePath -> IO TermList
csvMapTermList fp = csv2list CsvMap <$> snd <$>  fromCsvListFile fp

csv2list :: CsvListType -> Vector CsvList -> TermList
csv2list lt vs = V.toList $ V.map (\(CsvList _ label forms)
                           -> (DT.words label, [DT.words label] <> (filter (not . null) . map DT.words $ DT.splitOn csvListFormsDelimiter forms)))
                         $ V.filter (\l -> csvList_status l == lt ) vs

------------------------------------------------------------------------
data CsvListType = CsvMap | CsvStop | CsvCandidate
  deriving (Read, Show, Eq)
------------------------------------------------------------------------
-- CSV List Main Configuration
csvListFieldDelimiter :: Char
csvListFieldDelimiter = '\t'

csvListFormsDelimiter :: Text
csvListFormsDelimiter = "|&|"
------------------------------------------------------------------------
data CsvList = CsvList
    { csvList_status :: !CsvListType
    , csvList_label  :: !Text
    , csvList_forms  :: !Text
    }
    deriving (Show)
------------------------------------------------------------------------
instance FromNamedRecord CsvList where
  parseNamedRecord r = CsvList <$> r .: "status"
                               <*> r .: "label"
                               <*> r .: "forms"

instance ToNamedRecord CsvList where
  toNamedRecord (CsvList s l f) =
    namedRecord [ "status" .= s
                , "label"  .= l
                , "forms"  .= f
                ]
------------------------------------------------------------------------
instance FromField CsvListType where
    parseField "map"       = pure CsvMap
    parseField "main"      = pure CsvCandidate
    parseField "candidate" = pure CsvCandidate -- backward compat
    parseField "stop"      = pure CsvStop
    parseField _           = mzero

instance ToField CsvListType where
    toField CsvMap       = "map"
    toField CsvCandidate = "main"
    toField CsvStop      = "stop"
------------------------------------------------------------------------
csvDecodeOptions :: DecodeOptions
csvDecodeOptions = (defaultDecodeOptions
                      {decDelimiter = fromIntegral $ ord csvListFieldDelimiter}
                    )

csvEncodeOptions :: EncodeOptions
csvEncodeOptions = ( defaultEncodeOptions 
                      {encDelimiter = fromIntegral $ ord csvListFieldDelimiter}
                    )
------------------------------------------------------------------------
fromCsvListFile :: FilePath -> IO (Header, Vector CsvList)
fromCsvListFile fp = do
    csvData <- BL.readFile fp
    case decodeByNameWith csvDecodeOptions csvData of
      Left e        -> panicTrace (pack e)
      Right csvList -> pure csvList
------------------------------------------------------------------------
toCsvListFile :: FilePath -> (Header, Vector CsvList) -> IO ()
toCsvListFile fp (h, vs) = BL.writeFile fp $
                      encodeByNameWith csvEncodeOptions h (V.toList vs)
------------------------------------------------------------------------
