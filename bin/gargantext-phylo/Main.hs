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
import Data.Text  (unpack, replace, pack)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import GHC.IO.Encoding
import Gargantext.API.Ngrams.Prelude (toTermList)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Text.Context (TermList)
import Gargantext.Core.Text.Corpus.Parsers (FileFormat(..), FileType(..), parseFile)
import Gargantext.Core.Text.Corpus.Parsers.CSV (csv_title, csv_abstract, csv_publication_year, csv_publication_month, csv_publication_day, csv'_source, csv'_title, csv'_abstract, csv'_publication_year, csv'_publication_month, csv'_publication_day, csv'_weight)
import Gargantext.Core.Text.Corpus.Parsers.CSV qualified as Csv
import Gargantext.Core.Text.List.Formats.CSV (csvMapTermList)
import Gargantext.Core.Text.Ngrams (NgramsType(..))
import Gargantext.Core.Text.Terms.WithList (Patterns, buildPatterns, extractTermsWithList)
import Gargantext.Core.Types.Main (ListType(..))
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.API.Tools
import Gargantext.Core.Viz.Phylo.PhyloExport (toPhyloExport, dotToFile)
import Gargantext.Core.Viz.Phylo.PhyloMaker  (toPhylo, toPhyloWithoutLink)
import Gargantext.Core.Viz.Phylo.PhyloTools  (printIOMsg, printIOComment, setConfig, toPeriods, getTimePeriod, getTimeStep)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude hiding (hash, replace)
import Prelude qualified
import System.Directory (listDirectory,doesFileExist)
import Common

main :: IO ()
main = do

    setLocaleEncoding utf8
    currentLocale <- getLocaleEncoding
    printIOMsg $ "Machine locale: " <> show currentLocale
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
