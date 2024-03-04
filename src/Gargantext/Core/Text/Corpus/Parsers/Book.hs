{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.Book
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Get Book into GarganText

-}

module Gargantext.Core.Text.Corpus.Parsers.Book
  where

import Data.ByteString.Lazy qualified as DBL
import Data.List qualified as List
import Data.Text qualified as DT
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Parsers.CSV (hyperdataDocument2csv)
import Gargantext.Core.Text.Corpus.Parsers.FrameWrite (text2titleParagraphs)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude
import System.Directory -- (getDirectoryContents)

------------------------------------------------------------------------
-- Main Export Function

type FileOut = FilePath

book2csv :: Int -> FileDir -> FileOut -> IO ()
book2csv n f_in f_out = do
  files <- filesOf f_in
  texts <- readPublis f_in files
  let publis = concatMap (file2publi n) texts
  let docs = zipWith publiToHyperdata [1..] publis
  DBL.writeFile f_out (hyperdataDocument2csv docs)

filesOf :: FileDir -> IO [FilePath]
filesOf fd = List.sort                                        -- sort by filename
        <$>  List.filter (\f -> DT.length (cs f) > 2)
        <$>  getDirectoryContents fd

readPublis :: FileDir -> [FilePath] -> IO [(FilePath, Text)]
readPublis fd = mapM (\fp -> DBL.readFile (fd <> fp) >>= \txt -> pure (fp, cs txt))

------------------------------------------------------------------------
-- Main Types
data Publi = Publi { publi_authors :: [Text]
                   , publi_source  :: Text
                   , publi_title   :: Text
                   , publi_text    :: Text
                   }
  deriving (Show)

data FileInfo = FileInfo { fi_authors :: [Text]
                         , fi_source  :: Text
                         }
  deriving (Show)

type FileDir = FilePath
---------------------------------------------------------------------

file2publi :: Int -> (FilePath, Text) -> [Publi]
file2publi n (fp,theText) = map (uncurry (Publi authors source)) theTexts
  where
    theTexts = text2titleParagraphs n theText
    FileInfo authors source = fileNameInfo fp

fileNameInfo :: FilePath -> FileInfo
fileNameInfo fp = toFileInfo xs
  where
    xs = DT.splitOn "_" $ DT.pack fp
    toFileInfo (a:b:_) = FileInfo (DT.splitOn "-and-" a) (cs b)
    toFileInfo _       = panicTrace "error"

---------------------------------------------------------------------
publiToHyperdata :: Int -> Publi -> HyperdataDocument
publiToHyperdata y (Publi a s t txt) =
       HyperdataDocument { _hd_bdd = Just "Book File"
                        , _hd_doi = Nothing
                        , _hd_url = Nothing
                        , _hd_page = Nothing
                        , _hd_title = Just t
                        , _hd_authors = Just (DT.concat a)
                        , _hd_institutes = Nothing
                        , _hd_source = Just s
                        , _hd_abstract = Just txt
                        , _hd_publication_date = Nothing
                        , _hd_publication_year = Just y
                        , _hd_publication_month = Just 1
                        , _hd_publication_day = Just 1
                        , _hd_publication_hour = Nothing
                        , _hd_publication_minute = Nothing
                        , _hd_publication_second = Nothing
                        , _hd_language_iso2 = Just $ DT.pack $ show FR
                        }

-------------------------------------------------------------
-- MISC tool to remove urls for instance
clean :: Text -> Text
clean = DT.unwords . List.filter (\w -> DT.length w < 20) . DT.words
