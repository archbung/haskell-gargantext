{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Offline.Phylo (tests) where

import Data.GraphViz.Attributes.Complete qualified as Graphviz
import Data.Vector qualified as V
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.API.Tools (readPhylo, writePhylo)
import Gargantext.Core.Viz.Phylo.PhyloExport
import Gargantext.Core.Viz.Phylo.PhyloMaker (toPhylo)
import Gargantext.Core.Viz.Phylo.PhyloTools
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Text.Lazy as TL

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

tests :: TestTree
tests = testGroup "Phylo" [
  -- testCase "returns expected data" testSmallPhyloExpectedOutput
  testGroup "Export" [
      testCase "ngramsToLabel respects encoding" test_ngramsToLabel_01
    , testCase "ngramsToLabel is rendered correctly in CustomAttribute" test_ngramsToLabel_02
    ]
  ]

testSmallPhyloExpectedOutput :: Assertion
testSmallPhyloExpectedOutput = do
  issue290PhyloSmall <- setConfig phyloConfig <$> (readPhylo =<< getDataFileName "bench-data/phylo/issue-290-small.json")
  expected <- readPhylo =<< getDataFileName "test-data/phylo/issue-290-small.golden.json"
  let actual = toPhylo issue290PhyloSmall
  expected @?= actual

test_ngramsToLabel_01 :: Assertion
test_ngramsToLabel_01 =
  let ngrams = V.fromList [ "évaluation", "méthodologique" ]
  in ngramsToLabel ngrams [0,1] @?= "évaluation | méthodologique"

test_ngramsToLabel_02 :: Assertion
test_ngramsToLabel_02 =
  let ngrams = V.fromList [ "钱", "狗" ]
  in (Graphviz.customValue $ toAttr "lbl" $ TL.fromStrict $ ngramsToLabel ngrams [0,1]) @?= "钱 | 狗"
