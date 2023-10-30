
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Offline.JSON (tests) where

import Data.Aeson
import Data.Either
import Gargantext.API.Errors
import Gargantext.API.Node.Corpus.New
import Gargantext.API.Node.Corpus.Types
import Gargantext.Core.Types.Phylo
import Gargantext.Core.Viz.Phylo.API
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.RawString.QQ
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as C8

import Paths_gargantext
import Gargantext.Database.Admin.Types.Node

jsonRoundtrip :: (Show a, FromJSON a, ToJSON a, Eq a) => a -> Property
jsonRoundtrip a =
  counterexample ("Parsed JSON: " <> C8.unpack (encode a)) $ eitherDecode (encode a) === Right a

tests :: TestTree
tests = testGroup "JSON" [
    testProperty "NodeId roundtrips"        (jsonRoundtrip @NodeId)
  , testProperty "RootId roundtrips"        (jsonRoundtrip @RootId)
  , testProperty "Datafield roundtrips"     (jsonRoundtrip @Datafield)
  , testProperty "WithQuery roundtrips"     (jsonRoundtrip @WithQuery)
  , testProperty "FrontendError roundtrips" (jsonRoundtrip @FrontendError)
  , testCase "WithQuery frontend compliance" testWithQueryFrontend
  , testGroup "Phylo" [
    testProperty "PeriodToNode"  (jsonRoundtrip @PeriodToNodeData)
  , testProperty "GraphData"     (jsonRoundtrip @GraphData)
  , testProperty "GraphDataData" (jsonRoundtrip @GraphDataData)
  , testProperty "ObjectData"    (jsonRoundtrip @ObjectData)
  , testProperty "PhyloData"     (jsonRoundtrip @PhyloData)
  , testProperty "LayerData"     (jsonRoundtrip @LayerData)
  , testCase "can parse bpa_phylo_test.json" testParseBpaPhylo
  , testCase "can parse open_science.json" testOpenSciencePhylo
  ]
  ]

testWithQueryFrontend :: Assertion
testWithQueryFrontend = do
  case  eitherDecode @WithQuery (C8.pack cannedWithQueryPayload) of
   Left err -> fail $ "JSON instance will break frontend!: JSON decoding returned: " <> err
   Right _ -> pure ()

-- The aim of this type is to catch regressions in the frontend serialisation; this
-- is what the frontend currently expects, and therefore if we were to change the JSON
-- instances, this test would fail, and we will be notified.
cannedWithQueryPayload :: String
cannedWithQueryPayload = [r| {"query":"Haskell","node_id":138,"lang":"EN","flowListWith":{"type":"MyListsFirst"},"datafield": { "External": "Arxiv"},"databases":"Arxiv"} |]

testParseBpaPhylo :: Assertion
testParseBpaPhylo = do
  pth      <- getDataFileName "test-data/phylo/bpa_phylo_test.json"
  jsonBlob <- B.readFile pth
  case eitherDecodeStrict' @GraphData jsonBlob of
    Left err    -> error err
    Right _     -> pure ()

testOpenSciencePhylo :: Assertion
testOpenSciencePhylo = do
  pth      <- getDataFileName "test-data/phylo/open_science.json"
  jsonBlob <- B.readFile pth
  case eitherDecodeStrict' @PhyloData jsonBlob of
    Left err    -> error err
    Right _     -> pure ()
