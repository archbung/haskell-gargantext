{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import Control.DeepSeq
import Gargantext.Core.Types.Individu
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.API.Tools (readPhylo)
import Gargantext.Core.Viz.Phylo.PhyloMaker (toPhylo)
import Gargantext.Core.Viz.Phylo.PhyloTools
import Gargantext.Prelude.Crypto.Auth (createPasswordHash)

import Test.Tasty.Bench
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

main :: IO ()
main = do
  _issue290Phylo     <- force . setConfig phyloConfig <$> (readPhylo =<< getDataFileName "bench-data/phylo/issue-290.json")
  issue290PhyloSmall <- force . setConfig phyloConfig <$> (readPhylo =<< getDataFileName "bench-data/phylo/issue-290-small.json")
  defaultMain
    [ bgroup "Benchmarks"
      [ bgroup "User creation" [
        bench "createPasswordHash"  $ whnfIO (createPasswordHash "rabbit")
      , bench "toUserHash"  $
          whnfIO (toUserHash $ NewUser "alfredo" "alfredo@well-typed.com" (GargPassword "rabbit"))
      ]
      , bgroup "Phylo" [
          bench "toPhylo (small)" $ nf toPhylo issue290PhyloSmall
      ]
      ]
    ]
