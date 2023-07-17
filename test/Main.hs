{-|
Module      : Main.hs
Description : Main for Gargantext Tests
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


import Gargantext.Prelude

import qualified Core.Text.Corpus.Query  as CorpusQuery
import qualified Core.Utils              as Utils
import qualified Database.Operations     as DB
import qualified Graph.Clustering        as Graph
import qualified Ngrams.NLP              as NLP
import qualified Ngrams.Query            as NgramsQuery
import qualified Offline.JSON            as JSON
import qualified Parsers.Date            as PD
import qualified Utils.Crypto            as Crypto
import qualified Utils.Jobs              as Jobs

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  utilSpec         <- testSpec "Utils" Utils.test
  clusteringSpec   <- testSpec "Graph Clustering" Graph.test
  dateParserSpec   <- testSpec "Date Parsing" PD.testFromRFC3339
  cryptoSpec       <- testSpec "Crypto" Crypto.test
  nlpSpec          <- testSpec "NLP" NLP.test
  jobsSpec         <- testSpec "Jobs" Jobs.test

  defaultMain $ testGroup "Gargantext"
    [ utilSpec
    , clusteringSpec
    , dateParserSpec
    , cryptoSpec
    , nlpSpec
    , jobsSpec
    , NgramsQuery.tests
    , CorpusQuery.tests
    , JSON.tests
    , DB.tests
    ]
