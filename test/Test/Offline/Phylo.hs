{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Offline.Phylo (tests) where

import Data.Aeson
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.API.Tools (readPhylo)
import Gargantext.Core.Viz.Phylo.PhyloMaker (toPhylo, toPhyloWithoutLink)
import Gargantext.Core.Viz.Phylo.PhyloTools
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Common

import Paths_gargantext

phyloConfig :: PhyloConfig
phyloConfig = PhyloConfig {
    corpusPath = "corpus.csv"
  , listPath = "list.csv"
  , outputPath = "data/"
  , corpusParser = Csv {_csv_limit = 150000}
  , listParser = V4
  , phyloName = "Phylo Name"
  , phyloScale = 2
  , similarity = WeightedLogJaccard {_wlj_sensibility = 0.5, _wlj_minSharedNgrams = 2}
  , seaElevation = Constante {_cons_start = 0.1, _cons_gap = 0.1}
  , defaultMode = True
  , findAncestors = True
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

tests :: TestTree
tests = testGroup "Phylo" [
    testGroup "toPhyloWithoutLink" [
      testCase "returns expected data" testSmallPhyloWithoutLinkExpectedOutput
    ]
  , testGroup "toPhylo" [
    testCase "returns expected data" testSmallPhyloExpectedOutput
  ]
  , testGroup "relatedComponents" [
    testCase "finds simple connection" testRelComp_Connected
  ]
  ]

testSmallPhyloWithoutLinkExpectedOutput :: Assertion
testSmallPhyloWithoutLinkExpectedOutput = do
  bpaConfig      <- getDataFileName "bench-data/phylo/bpa-config.json"
  corpusPath'    <- getDataFileName "test-data/phylo/small_phylo_docslist.csv"
  listPath'      <- getDataFileName "test-data/phylo/small_phylo_ngramslist.csv"
  (Right config) <- fmap (\pcfg -> pcfg { corpusPath = corpusPath'
                                        , listPath   = listPath'
                                        }) <$> (eitherDecodeFileStrict' bpaConfig)
  mapList <- fileToList (listParser config) (listPath config)
  corpus <- fileToDocsDefault (corpusParser config)
                              (corpusPath config)
                              [Year 3 1 5,Month 3 1 5,Week 4 2 5]
                              mapList
  actual   <- pure $ toPhyloWithoutLink corpus config
  expected <- readPhylo =<< getDataFileName "test-data/phylo/small-phylo.golden.json"
  expected @?= actual

testSmallPhyloExpectedOutput :: Assertion
testSmallPhyloExpectedOutput = do
  issue290PhyloSmall <- setConfig phyloConfig <$> (readPhylo =<< getDataFileName "bench-data/phylo/issue-290-small.json")
  expected <- readPhylo =<< getDataFileName "test-data/phylo/issue-290-small.golden.json"
  let actual = toPhylo issue290PhyloSmall
  expected @?= actual

testRelComp_Connected :: Assertion
testRelComp_Connected = do
  (relatedComponents @Int) []                                @?= []
  (relatedComponents @Int) [[]]                              @?= [[]]
  (relatedComponents @Int) [[],[1,2]]                        @?= [[],[1,2]]
  (relatedComponents @Int) [[1,2],[]]                        @?= [[1,2],[]]
  (relatedComponents @Int) [[1,2], [2]]                      @?= [[1,2]]
  (relatedComponents @Int) [[1,2], [2],[2]]                  @?= [[1,2]]
  (relatedComponents @Int) [[1,2], [2],[2,1]]                @?= [[1,2]]
  (relatedComponents @Int) [[1,2], [2,4]]                    @?= [[1,2,4]]
  (relatedComponents @Int) [[1,2], [3,5], [2,4]]             @?= [[3,5], [1,2,4]]
  (relatedComponents @Int) [[1,2], [3,5], [2,4],[9,5],[5,4]] @?= [[1,2,4,3,5,9]]
  (relatedComponents @Int) [[1,2,5], [4,5,9]]                @?= [[1,2,5,4,9]]

