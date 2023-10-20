{--|
Module      : Main.hs
Description : Main for Gargantext Tasty Tests
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Main where

import Gargantext.Prelude

import qualified Test.Core.Text.Corpus.Query  as CorpusQuery
import qualified Test.Core.Utils              as Utils
import qualified Test.Graph.Clustering        as Graph
import qualified Test.Ngrams.NLP              as NLP
import qualified Test.Ngrams.Query            as NgramsQuery
import qualified Test.Offline.JSON            as JSON
import qualified Test.Parsers.Date            as PD
import qualified Test.Utils.Crypto            as Crypto
import qualified Test.Utils.Jobs              as Jobs

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  utilSpec         <- testSpec "Utils" Utils.test
  clusteringSpec   <- testSpec "Graph Clustering" Graph.test
  dateParserSpec   <- testSpec "Date Parsing" PD.testFromRFC3339
  dateSplitSpec    <- testSpec "Date split" PD.testDateSplit
  cryptoSpec       <- testSpec "Crypto" Crypto.test
  nlpSpec          <- testSpec "NLP" NLP.test
  jobsSpec         <- testSpec "Jobs" Jobs.test

  defaultMain $ testGroup "Gargantext"
    [ utilSpec
    , clusteringSpec
    , dateParserSpec
    , dateSplitSpec
    , cryptoSpec
    , nlpSpec
    , jobsSpec
    , NgramsQuery.tests
    , CorpusQuery.tests
    , JSON.tests
    ]
