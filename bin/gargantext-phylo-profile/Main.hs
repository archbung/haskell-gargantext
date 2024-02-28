{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common
import Data.Aeson
import Data.List (nub)
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.API.Tools
import Gargantext.Core.Viz.Phylo.PhyloExport (toPhyloExport, dotToFile)
import Gargantext.Core.Viz.Phylo.PhyloMaker  (toPhylo, toPhyloWithoutLink)
import Gargantext.Core.Viz.Phylo.PhyloTools  (printIOMsg, printIOComment, setConfig, toPeriods, getTimePeriod, getTimeStep)
import GHC.IO.Encoding
import GHC.Stack
import Paths_gargantext
import Prelude
import qualified Data.Text as T
import Shelly
import System.Directory

--------------
-- | Main | --
--------------

phyloConfig :: FilePath -> PhyloConfig
phyloConfig outdir = PhyloConfig {
    corpusPath = "corpus.csv"
  , listPath = "list.csv"
  , outputPath = outdir
  , corpusParser = Csv {_csv_limit = 150000}
  , listParser = V4
  , phyloName = "phylo_profile_test"
  , phyloScale = 2
  , similarity = WeightedLogJaccard {_wlj_sensibility = 0.5, _wlj_minSharedNgrams = 2}
  , seaElevation = Constante {_cons_start = 0.1, _cons_gap = 0.1}
  , defaultMode = True
  , findAncestors = False
  , phyloSynchrony = ByProximityThreshold {_bpt_threshold = 0.5, _bpt_sensibility = 0.0, _bpt_scope = AllBranches, _bpt_strategy = MergeAllGroups}
  , phyloQuality = Quality {_qua_granularity = 0.8, _qua_minBranch = 3}
  , timeUnit = Year {_year_period = 3, _year_step = 1, _year_matchingFrame = 5}
  , clique = MaxClique {_mcl_size = 5, _mcl_threshold = 1.0e-4, _mcl_filter = ByThreshold}
  , exportLabel = [ BranchLabel {_branch_labelTagger = MostEmergentTfIdf, _branch_labelSize = 2}
                  , GroupLabel {_group_labelTagger = MostEmergentInclusive, _group_labelSize = 2}
                  ]
  , exportSort = ByHierarchy {_sort_order = Desc}
  , exportFilter = [ByBranchSize {_branch_size = 3.0}]
  }


main :: HasCallStack => IO ()
main = do

    shelly $ escaping False $ withTmpDir $ \tdir -> do
      curDir <- pwd
      let output = curDir <> "/" <> "gargantext_profile_out.dot"
      chdir tdir $ do
        liftIO $ setLocaleEncoding utf8

        bpaConfig      <- liftIO $ getDataFileName "bench-data/phylo/bpa-config.json"
        corpusPath'    <- liftIO $ getDataFileName "bench-data/phylo/GarganText_DocsList-nodeId-185487.csv"
        listPath'      <- liftIO $ getDataFileName "bench-data/phylo/GarganText_NgramsList-185488.csv"
        (Right config) <- fmap (\pcfg -> pcfg { outputPath = tdir
                                              , corpusPath = corpusPath'
                                              , listPath   = listPath'
                                              }) <$> liftIO (eitherDecodeFileStrict' bpaConfig)

        mapList <-  liftIO $ fileToList (listParser config) (listPath config)

        corpus  <- liftIO $ if (defaultMode config)
                    then fileToDocsDefault (corpusParser config) (corpusPath config) [Year 3 1 5,Month 3 1 5,Week 4 2 5] mapList
                    else fileToDocsAdvanced (corpusParser config) (corpusPath config) (timeUnit config)  mapList

        liftIO $ do
          printIOComment (show (length corpus) <> " parsed docs from the corpus")
          printIOComment (show (length $ nub $ concat $ map text corpus) <> " Size ngs_coterms")
          printIOComment (show (length mapList) <> " Size ngs_terms List Map Ngrams")
          printIOMsg "Reconstruct the phylo"

          -- check the existing backup files

          let backupPhyloWithoutLink = (outputPath config) <> "backupPhyloWithoutLink_" <> (configToSha BackupPhyloWithoutLink config) <> ".json"
          let backupPhylo = (outputPath config) <> "backupPhylo_"   <> (configToSha BackupPhylo config) <> ".json"

          phyloWithoutLinkExists <- doesFileExist backupPhyloWithoutLink
          phyloExists            <- doesFileExist backupPhylo

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

          dotToFile output dot
        echo "Done."
