{-|
Module      : Gargantext.Core.Text.Corpus.Parsers
Description : All parsers of Gargantext in one file.
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Gargantext enables analyzing semi-structured text that should be parsed
in order to be analyzed.

The parsers suppose we know the format of the Text (TextFormat data
type) according to which the right parser is chosen among the list of
available parsers.

This module mainly describe how to add a new parser to Gargantext,
please follow the types.
-}

{-# LANGUAGE PackageImports    #-}

module Gargantext.Core.Text.Corpus.Parsers (FileFormat(..), FileType(..), clean, parseFile, cleanText, parseFormatC, splitOn, etale)
    where

-- import Gargantext.Core.Text.Learn (detectLangDefault)
import "zip" Codec.Archive.Zip (EntrySelector, withArchive, getEntry, getEntries, unEntrySelector)
import Conduit
import Control.Concurrent.Async as CCA (mapConcurrently)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Attoparsec.ByteString (parseOnly, Parser)
import Data.ByteString qualified as DB
import Data.ByteString.Char8 qualified as DBC
import Data.ByteString.Lazy qualified as DBL
import Data.List (lookup)
import Data.Map qualified as DM
import Data.Text qualified as DT
import Data.Tuple.Extra (both) -- , first, second)
import Gargantext.API.Node.Corpus.New.Types (FileFormat(..))
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Parsers.CSV (parseHal, parseCsv, parseCsvC)
import Gargantext.Core.Text.Corpus.Parsers.Date qualified as Date
import Gargantext.Core.Text.Corpus.Parsers.FrameWrite (text2titleParagraphs)
import Gargantext.Core.Text.Corpus.Parsers.Iramuteq qualified as Iramuteq
import Gargantext.Core.Text.Corpus.Parsers.JSON (parseJSONC, parseIstex)
import Gargantext.Core.Text.Corpus.Parsers.RIS qualified as RIS
import Gargantext.Core.Text.Corpus.Parsers.RIS.Presse (presseEnrich)
import Gargantext.Core.Text.Corpus.Parsers.WOS qualified as WOS
import Gargantext.Core.Text.Ngrams (NgramsType(..))
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument(..) )
import Gargantext.Prelude hiding (show, undefined)
import Gargantext.Utils.Zip qualified as UZip
import Protolude ( show )
import System.FilePath (takeExtension)
------------------------------------------------------------------------

type ParseError = Text
--type Field      = Text
--type Document   = DM.Map Field Text
--type FilesParsed = DM.Map FilePath FileParsed
--data FileParsed  = FileParsed { _fileParsed_errors ::  Maybe ParseError
--                              , _fileParsed_result :: [Document]
--                              } deriving (Show)


-- | According to the format of Input file,
-- different parser are available.
data FileType = WOS
              | RIS
              | RisPresse
              | CsvGargV3
              | CsvHal
              | Iramuteq
              | JSON
              | Istex
  deriving (Show, Eq)

-- Implemented (ISI Format)
--                | DOC        -- Not Implemented / import Pandoc
--                | ODT        -- Not Implemented / import Pandoc
--                | PDF        -- Not Implemented / pdftotext and import Pandoc ?
--                | XML        -- Not Implemented / see :

parseFormatC :: MonadBaseControl IO m
             => FileType
             -> FileFormat
             -> DB.ByteString
             -> m (Either Text (Integer, ConduitT () HyperdataDocument IO ()))
parseFormatC CsvGargV3 Plain bs = do
  let eParsedC = parseCsvC $ DBL.fromStrict bs
  pure (second (transPipe (pure . runIdentity)) <$> eParsedC)
parseFormatC CsvHal    Plain bs = do
  let eParsedC = parseCsvC $ DBL.fromStrict bs
  pure (second (transPipe (pure . runIdentity)) <$> eParsedC)
parseFormatC Istex Plain bs = do
  ep <- liftBase $ parseIstex EN $ DBL.fromStrict bs
  pure $ (\p -> (1, yieldMany [p])) <$> ep
parseFormatC RisPresse Plain bs = do
  --docs <- enrichWith RisPresse
  let eDocs = runParser' RisPresse bs
  pure $ (\docs ->
            ( fromIntegral $ length docs
            , yieldMany docs
              .| mapC presseEnrich
              .| mapC (map $ both decodeUtf8)
              .| mapMC (toDoc RIS)) ) <$> eDocs
parseFormatC WOS Plain bs = do
  let eDocs = runParser' WOS bs
  pure $ (\docs ->
            ( fromIntegral $ length docs
            , yieldMany docs
              .| mapC (map $ first WOS.keys)
              .| mapC (map $ both decodeUtf8)
              .| mapMC (toDoc WOS)) ) <$> eDocs

parseFormatC Iramuteq Plain bs = do
  let eDocs = runParser' Iramuteq bs
  pure $ (\docs ->
            ( fromIntegral $ length docs
            , yieldMany docs
              .| mapC (map $ first Iramuteq.keys)
              .| mapC (map $ both decodeUtf8)
              .| mapMC (toDoc Iramuteq . map (second (DT.replace "_" " ")))
            )
         )
              <$> eDocs
parseFormatC JSON    Plain bs = do
  let eParsedC = parseJSONC $ DBL.fromStrict bs
  pure (second (transPipe (pure . runIdentity)) <$> eParsedC)
parseFormatC ft ZIP bs = liftBase $ UZip.withZipFileBS bs $ do
  fileNames <- filter (filterZIPFileNameP ft) . DM.keys <$> getEntries
  printDebug "[parseFormatC] fileNames" fileNames
  fileContents <- mapM getEntry fileNames
  --printDebug "[parseFormatC] fileContents" fileContents
  eContents <- mapM (parseFormatC ft Plain) fileContents
  --printDebug "[parseFormatC] contents" contents
  --pure $ Left $ "Not implemented for ZIP"
  let (errs, contents) = partitionEithers eContents
  case errs of
    [] ->
      case contents of
        [] -> pure $ Left "No files in zip"
        _  -> do
          let lenghts = fst <$> contents
          let contents' = snd <$> contents
          let totalLength = sum lenghts
          pure $ Right ( totalLength
                       , void (sequenceConduits contents') ) -- .| mapM_C (printDebug "[parseFormatC] doc")
    _ -> pure $ Left $ DT.intercalate "\n" errs
parseFormatC _ _ _ = pure $ Left "Not implemented"


filterZIPFileNameP :: FileType -> EntrySelector -> Bool
filterZIPFileNameP Istex f = (takeExtension (unEntrySelector f) == ".json") &&
                             (unEntrySelector f /= "manifest.json")
filterZIPFileNameP _     _ = True


etale :: [HyperdataDocument] -> [HyperdataDocument]
etale = concatMap etale'
  where
    etale' :: HyperdataDocument -> [HyperdataDocument]
    etale' h = map (\t -> h { _hd_abstract = Just t })
            $ map snd
            $ text2titleParagraphs 7 (maybe "" identity $ _hd_abstract h)


-- parseFormat :: FileType -> DB.ByteString -> IO (Either Prelude.String [HyperdataDocument])
-- parseFormat CsvGargV3 bs = pure $ parseCsv' $ DBL.fromStrict bs
-- parseFormat CsvHal    bs = pure $ parseHal' $ DBL.fromStrict bs
-- parseFormat RisPresse bs = do
--   docs <- mapM (toDoc RIS)
--           <$> snd
--           <$> enrichWith RisPresse
--           $ partitionEithers
--           $ [runParser'  RisPresse bs]
--   pure $ Right docs
-- parseFormat WOS bs = do
--   docs <- mapM (toDoc WOS)
--           <$> snd
--           <$> enrichWith WOS
--           $ partitionEithers
--           $ [runParser'  WOS bs]
--   pure $ Right docs
-- parseFormat ZIP bs = do
--   path <- emptySystemTempFile "parsed-zip"
--   DB.writeFile path bs
--   parsedZip <- withArchive path $ do
--     DM.keys <$> getEntries
--   pure $ Left $ "Not implemented for ZIP, parsedZip" <> show parsedZip
-- parseFormat _ _ = undefined

-- | Parse file into documents
-- TODO manage errors here
-- TODO: to debug maybe add the filepath in error message

parseFile :: FileType
          -> FileFormat
          -> FilePath
          -> IO (Either Text [HyperdataDocument])
parseFile CsvGargV3 Plain p = parseCsv p
parseFile CsvHal    Plain p = parseHal p
parseFile RisPresse Plain p = do
  docs <- join $ mapM (toDoc RIS) <$> snd <$> enrichWith RisPresse <$> readFileWith RIS p
  pure $ Right docs
parseFile WOS       Plain p = do
  docs <- join $ mapM (toDoc WOS) <$> snd <$> enrichWith WOS       <$> readFileWith WOS p
  pure $ Right docs
parseFile Iramuteq  Plain p = do
  docs <- join $ mapM ((toDoc Iramuteq) . (map (second (DT.replace "_" " "))))
              <$> snd
              <$> enrichWith Iramuteq
              <$> readFileWith Iramuteq p
  pure $ Right docs
parseFile Istex Plain p = do
  printDebug "[parseFile] Istex ZIP p" p
  pure $ Left "Error! Not supported yet!"
parseFile ff        _ p = do
  docs <- join $ mapM (toDoc ff) <$> snd <$> enrichWith ff        <$> readFileWith ff  p
  pure $ Right docs

toDoc :: FileType -> [(Text, Text)] -> IO HyperdataDocument
-- TODO use language for RIS
toDoc ff d = do
      -- let abstract = lookup "abstract" d
      let lang = EN -- maybe EN identity (join $ detectLangDefault <$> (fmap (DT.take 50) abstract))

      let dateToParse = DT.replace " " "" <$> lookup "PY" d  -- <> Just " " <> lookup "publication_date" d
      -- printDebug "[G.C.T.C.Parsers] dateToParse" dateToParse
      let (utcTime, (pub_year, pub_month, pub_day)) = Date.mDateSplit dateToParse

      let hd = HyperdataDocument { _hd_bdd = Just $ DT.pack $ show ff
                                 , _hd_doi = lookup "doi" d
                                 , _hd_url = lookup "URL" d
                                 , _hd_page = Nothing
                                 , _hd_title = lookup "title" d
                                 , _hd_authors = lookup "authors" d
                                 , _hd_institutes = lookup "institutes" d
                                 , _hd_source = lookup "source" d
                                 , _hd_abstract = lookup "abstract" d
                                 , _hd_publication_date = fmap (DT.pack . show) utcTime
                                 , _hd_publication_year = pub_year
                                 , _hd_publication_month = pub_month
                                 , _hd_publication_day = pub_day
                                 , _hd_publication_hour = Nothing
                                 , _hd_publication_minute = Nothing
                                 , _hd_publication_second = Nothing
                                 , _hd_language_iso2 = Just $ (DT.pack . show) lang }
      -- printDebug "[G.C.T.C.Parsers] HyperdataDocument" hd
      pure hd

enrichWith :: FileType
           ->  (a, [[[(DB.ByteString, DB.ByteString)]]]) -> (a, [[(Text, Text)]])
enrichWith RisPresse = enrichWith' presseEnrich
enrichWith WOS       = enrichWith' (map (first WOS.keys))
enrichWith Iramuteq  = enrichWith' (map (first Iramuteq.keys))
enrichWith _         = enrichWith' identity


enrichWith' ::      ([(DB.ByteString, DB.ByteString)] -> [(DB.ByteString, DB.ByteString)])
           ->  (a, [[[(DB.ByteString, DB.ByteString)]]]) -> (a, [[(Text, Text)]])
enrichWith' f = second (map both' . map f . concat)
  where
    both'   = map (both decodeUtf8)



readFileWith :: FileType -> FilePath
       -> IO ([ParseError], [[[(DB.ByteString, DB.ByteString)]]])
readFileWith format path = do
    files <- case takeExtension path of
              ".zip" -> openZip              path
              _      -> pure <$> clean <$> DB.readFile path
    partitionEithers <$> mapConcurrently (runParser format) files


-- | withParser:
-- According to the format of the text, choose the right parser.
-- TODO  withParser :: FileType -> Parser [Document]
withParser :: FileType -> Parser [[(DB.ByteString, DB.ByteString)]]
withParser WOS      = WOS.parser
withParser RIS      = RIS.parser
withParser Iramuteq = Iramuteq.parser
--withParser ODT = odtParser
--withParser XML = xmlParser
withParser _   = panicTrace "[ERROR] Parser not implemented yet"

runParser :: FileType -> DB.ByteString
          -> IO (Either Text [[(DB.ByteString, DB.ByteString)]])
runParser format text = pure $ runParser' format text

runParser' :: FileType
          -> DB.ByteString
          -> Either Text [[(DB.ByteString, DB.ByteString)]]
runParser' format text = first DT.pack $ parseOnly (withParser format) text

openZip :: FilePath -> IO [DB.ByteString]
openZip fp = do
    entries <- withArchive fp (DM.keys <$> getEntries)
    bs      <- mapConcurrently (\s -> withArchive fp (getEntry s)) entries
    pure bs

cleanText :: Text -> Text
cleanText = cs . clean . cs

clean :: DB.ByteString -> DB.ByteString
clean txt = DBC.map clean' txt
  where
    clean' '’' = '\''
    clean' '\r' = ' '
    clean' '\t' = ' '
    clean' ';' = '.'
    clean' c  = c

--

splitOn :: NgramsType -> Maybe Text -> Text -> [Text]
splitOn Authors (Just "WOS") = DT.splitOn "; "
splitOn _ _                  = DT.splitOn ", "
