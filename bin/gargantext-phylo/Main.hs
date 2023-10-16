{-|
Module      : Main.hs
Description : Gargantext starter binary with Adaptative Phylo
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Adaptative Phylo binaries
 -}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE Strict             #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Crypto.Hash.SHA256 (hash)
import Data.Aeson
import Data.ByteString.Char8 qualified as C8
import Data.List  (nub, isSuffixOf, tail)
import Data.List.Split
import Data.Maybe (fromJust)
import Data.Text  (unwords, unpack, replace, pack)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import GHC.IO (FilePath)
import Gargantext.API.Ngrams.Prelude (toTermList)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Text.Context (TermList)
import Gargantext.Core.Text.Corpus.Parsers (FileFormat(..), FileType(..), parseFile)
import Gargantext.Core.Text.Corpus.Parsers.CSV (csv_title, csv_abstract, csv_publication_year, csv_publication_month, csv_publication_day, csv'_source, csv'_title, csv'_abstract, csv'_publication_year, csv'_publication_month, csv'_publication_day, csv'_weight)
import Gargantext.Core.Text.Corpus.Parsers.CSV qualified as Csv
import Gargantext.Core.Text.List.Formats.CSV (csvMapTermList)
import Gargantext.Core.Text.Terms.WithList (Patterns, buildPatterns, extractTermsWithList)
import Gargantext.Core.Types.Main (ListType(..))
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.API.Tools
import Gargantext.Core.Viz.Phylo.PhyloExport (toPhyloExport, dotToFile)
import Gargantext.Core.Viz.Phylo.PhyloMaker  (toPhylo, toPhyloWithoutLink)
import Gargantext.Core.Viz.Phylo.PhyloTools  (printIOMsg, printIOComment, setConfig, toPeriods, getTimePeriod, getTimeStep)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude hiding (hash, replace)
import Prelude qualified
import System.Directory (listDirectory,doesFileExist)
import System.Environment

data Backup = BackupPhyloWithoutLink | BackupPhylo deriving (Show)

---------------
-- | Tools | --
---------------

-- | To get all the files in a directory or just a file
getFilesFromPath :: FilePath -> IO [FilePath]
getFilesFromPath path = do
  if (isSuffixOf "/" path)
    then (listDirectory path)
    else return [path]

----------------
-- | Parser | --
----------------

-- | To filter the Ngrams of a document based on the termList
termsInText :: Patterns -> Text -> [Text]
termsInText pats txt = nub $ concat $ map (map unwords) $ extractTermsWithList pats txt


-- | To transform a Wos file (or [file]) into a list of Docs
wosToDocs :: Int -> Patterns -> TimeUnit -> FilePath -> IO [Document]
wosToDocs limit patterns time path = do
      files <- getFilesFromPath path
      take limit
        <$> map (\d -> let title = fromJust $ _hd_title d
                           abstr = if (isJust $ _hd_abstract d)
                                   then fromJust $ _hd_abstract d
                                   else ""
                        in Document (toPhyloDate
                                      (fromIntegral $ fromJust $ _hd_publication_year d)
                                      (fromJust $ _hd_publication_month d)
                                      (fromJust $ _hd_publication_day d) time)
                                    (toPhyloDate'
                                      (fromIntegral $ fromJust $ _hd_publication_year d)
                                      (fromJust $ _hd_publication_month d)
                                      (fromJust $ _hd_publication_day d) time)
                                    (termsInText patterns $ title <> " " <> abstr) Nothing [] time)
        <$> concat
        <$> mapConcurrently (\file ->
              filter (\d -> (isJust $ _hd_publication_year d)
                         && (isJust $ _hd_title d))
                <$> fromRight [] <$> parseFile WOS Plain (path <> file) ) files


-- To transform a Csv file into a list of Document
csvToDocs :: CorpusParser -> Patterns -> TimeUnit -> FilePath -> IO [Document]
csvToDocs parser patterns time path =
  case parser of
    Wos  _     -> undefined
    Csv  limit -> Vector.toList
      <$> Vector.take limit
      <$> Vector.map (\row -> Document (toPhyloDate  (Csv.fromMIntOrDec Csv.defaultYear $ csv_publication_year row) (fromMaybe Csv.defaultMonth $ csv_publication_month row) (fromMaybe Csv.defaultDay $ csv_publication_day row) time)
                                       (toPhyloDate' (Csv.fromMIntOrDec Csv.defaultYear $ csv_publication_year row) (fromMaybe Csv.defaultMonth $ csv_publication_month row) (fromMaybe Csv.defaultDay $ csv_publication_day row) time)
                                       (termsInText patterns $ (csv_title row) <> " " <> (csv_abstract row))
                                       Nothing
                                       []
                                       time
                     ) <$> snd <$> either (\err -> panic $ "CSV error" <> (show err)) identity <$> Csv.readCSVFile path
    Csv' limit -> Vector.toList
      <$> Vector.take limit
      <$> Vector.map (\row -> Document (toPhyloDate  (csv'_publication_year row) (csv'_publication_month row) (csv'_publication_day row) time)
                                       (toPhyloDate' (csv'_publication_year row) (csv'_publication_month row) (csv'_publication_day row) time)
                                       (termsInText patterns $ (csv'_title row) <> " " <> (csv'_abstract row))
                                       (Just $ csv'_weight row)
                                       (map (T.strip . pack) $ splitOn ";" (unpack $ (csv'_source row)))
                                       time
                     ) <$> snd <$> Csv.readWeightedCsv path


-- To parse a file into a list of Document
fileToDocsAdvanced :: CorpusParser -> FilePath -> TimeUnit -> TermList -> IO [Document]
fileToDocsAdvanced parser path time lst = do
  let patterns = buildPatterns lst
  case parser of
      Wos limit  -> wosToDocs limit  patterns time path
      Csv  _     -> csvToDocs parser patterns time path
      Csv' _     -> csvToDocs parser patterns time path

fileToDocsDefault :: CorpusParser -> FilePath -> [TimeUnit] -> TermList -> IO [Document]
fileToDocsDefault parser path timeUnits lst = 
  if length timeUnits > 0
    then
      do 
        let timeUnit = (head' "fileToDocsDefault" timeUnits)
        docs <- fileToDocsAdvanced parser path timeUnit lst  
        let periods = toPeriods (sort $ nub $ map date docs) (getTimePeriod timeUnit) (getTimeStep timeUnit)
        if (length periods < 3)
         then fileToDocsDefault parser path (tail timeUnits) lst
         else pure docs
    else panic "this corpus is incompatible with the phylomemy reconstruction"

-- on passe à passer la time unit dans la conf envoyé au phyloMaker
-- dans le phyloMaker si default est true alors dans le setDefault ou pense à utiliser la TimeUnit de la conf 


---------------
-- | Label | --
---------------


-- Config time parameters to label
timeToLabel :: PhyloConfig -> [Char]
timeToLabel config = case (timeUnit config) of
      Epoch p s f -> ("time_epochs" <> "_" <> (show p) <> "_" <> (show s) <> "_" <> (show f))
      Year  p s f -> ("time_years"  <> "_" <> (show p) <> "_" <> (show s) <> "_" <> (show f))
      Month p s f -> ("time_months" <> "_" <> (show p) <> "_" <> (show s) <> "_" <> (show f))
      Week  p s f -> ("time_weeks"  <> "_" <> (show p) <> "_" <> (show s) <> "_" <> (show f))
      Day   p s f -> ("time_days"   <> "_" <> (show p) <> "_" <> (show s) <> "_" <> (show f))


seaToLabel :: PhyloConfig -> [Char]
seaToLabel config = case (seaElevation config) of
      Constante start step   -> ("sea_cst_"  <> (show start) <> "_" <> (show step))
      Adaptative granularity -> ("sea_adapt" <> (show granularity))
      Evolving _ -> ("sea_evolv")


sensToLabel :: PhyloConfig -> [Char]
sensToLabel config = case (similarity config) of
      Hamming _ _ -> undefined
      WeightedLogJaccard s _ -> ("WeightedLogJaccard_"  <> show s)
      WeightedLogSim s _ -> ( "WeightedLogSim-sens_"  <> show s)


cliqueToLabel :: PhyloConfig -> [Char]
cliqueToLabel config = case (clique config) of
      Fis s s' -> "fis_" <> (show s) <> "_" <> (show s')
      MaxClique s t f ->  "clique_" <> (show s)<> "_"  <> (show f)<> "_"  <> (show t)


syncToLabel :: PhyloConfig -> [Char]
syncToLabel config = case (phyloSynchrony config) of
      ByProximityThreshold scl sync_sens scope _ -> ("scale_" <> (show scope) <> "_" <> (show sync_sens)  <> "_"  <> (show scl))
      ByProximityDistribution _ _ -> undefined

qualToConfig :: PhyloConfig -> [Char]
qualToConfig config = case (phyloQuality config) of
      Quality g m -> "quality_" <> (show g) <> "_" <> (show m)


-- To set up the export file's label from the configuration
configToLabel :: PhyloConfig -> [Char]
configToLabel config = outputPath config
                    <> (unpack $ phyloName config)
                    <> "-" <> (timeToLabel config)
                    <> "-scale_" <> (show (phyloScale config))
                    <> "-" <> (seaToLabel config)
                    <> "-" <> (sensToLabel config)
                    <> "-" <> (cliqueToLabel config)
                    <> "-level_" <> (show (_qua_granularity $ phyloQuality config))
                    <> "-" <> (syncToLabel config)
                    <> ".dot"


-- To write a sha256 from a set of config's parameters
configToSha :: Backup -> PhyloConfig -> [Char]
configToSha stage config = unpack
                         $ replace "/" "-"
                         $ T.pack (show (hash $ C8.pack label))
  where
    label :: [Char]
    label = case stage of
      BackupPhyloWithoutLink -> (corpusPath    config)
                       <> (listPath      config)
                       <> (timeToLabel   config)
                       <> (cliqueToLabel config)
      BackupPhylo   -> (corpusPath    config)
                       <> (listPath      config)
                       <> (timeToLabel   config)
                       <> (cliqueToLabel config)
                       <> (sensToLabel   config)
                       <> (seaToLabel    config)
                       <> (syncToLabel   config)
                       <> (qualToConfig  config)
                       <> (show (phyloScale config))


readListV4 :: [Char] -> IO NgramsList
readListV4 path = do
  listJson <- (eitherDecode <$> readJson path) :: IO (Either Prelude.String NgramsList)
  case listJson of
    Left err -> do
      putStrLn err
      undefined
    Right listV4 -> pure listV4


fileToList  :: ListParser -> FilePath -> IO TermList
fileToList parser path =
  case parser of
    V3 -> csvMapTermList path
    V4 -> fromJust
      <$> toTermList MapTerm NgramsTerms
      <$> readListV4 path


--------------
-- | Main | --
--------------


main :: IO ()
main = do

    printIOMsg "Starting the reconstruction"

    printIOMsg "Read the configuration file"
    [args]   <- getArgs
    jsonArgs <- (eitherDecode <$> readJson args) :: IO (Either Prelude.String PhyloConfig)

    case jsonArgs of
        Left err     -> putStrLn err
        Right config -> do

            printIOMsg "Parse the corpus"
            mapList <-  fileToList (listParser config) (listPath config)

            corpus  <- if (defaultMode config)
                        then fileToDocsDefault (corpusParser config) (corpusPath config) [Year 3 1 5,Month 3 1 5,Week 4 2 5] mapList
                        else fileToDocsAdvanced (corpusParser config) (corpusPath config) (timeUnit config)  mapList
            
            printIOComment (show (length corpus) <> " parsed docs from the corpus")

            printIOComment (show (length $ nub $ concat $ map text corpus) <> " Size ngs_coterms")

            printIOComment (show (length mapList) <> " Size ngs_terms List Map Ngrams")

            printIOMsg "Reconstruct the phylo"

            -- check the existing backup files

            let backupPhyloWithoutLink = (outputPath config) <> "backupPhyloWithoutLink_" <> (configToSha BackupPhyloWithoutLink config) <> ".json"
            let backupPhylo = (outputPath config) <> "backupPhylo_"   <> (configToSha BackupPhylo config) <> ".json"

            phyloWithoutLinkExists <- doesFileExist backupPhyloWithoutLink
            phyloExists   <- doesFileExist backupPhylo

            -- reconstruct the phylo

            phylo <- if phyloExists
                        then do
                          printIOMsg "Reconstruct the phylo from an existing file"
                          readPhylo backupPhylo
                        else do
                          if phyloWithoutLinkExists
                            then do
                              printIOMsg "Reconstruct the phylo from an existing file without links"
                              phyloWithoutLink <- readPhylo backupPhyloWithoutLink
                              writePhylo backupPhyloWithoutLink phyloWithoutLink
                              pure $ toPhylo (setConfig config phyloWithoutLink)
                            else do
                              printIOMsg "Reconstruct the phylo from scratch"
                              phyloWithoutLink <- pure $ toPhyloWithoutLink corpus config
                              writePhylo backupPhyloWithoutLink phyloWithoutLink
                              pure $ toPhylo (setConfig config phyloWithoutLink)

            writePhylo backupPhylo phylo

            printIOMsg "End of reconstruction, start the export"

            let dot = toPhyloExport (setConfig config phylo)

            let output = configToLabel config

            dotToFile output dot
