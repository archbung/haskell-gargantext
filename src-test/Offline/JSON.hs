
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Offline.JSON (tests) where

import Data.Aeson
import Data.Either
import Gargantext.API.Node.Corpus.New
import Gargantext.API.Node.Corpus.Types
import Gargantext.Core.Viz.Phylo.API
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.RawString.QQ
import qualified Data.ByteString.Lazy.Char8 as C8

jsonRoundtrip :: (Show a, FromJSON a, ToJSON a, Eq a) => a -> Property
jsonRoundtrip a = eitherDecode (encode a) === Right a

tests :: TestTree
tests = testGroup "JSON" [
    testProperty "Datafield roundtrips" (jsonRoundtrip @Datafield)
  , testProperty "WithQuery roundtrips" (jsonRoundtrip @WithQuery)
  , testCase "WithQuery frontend compliance" testWithQueryFrontend
  , testGroup "Phylo" [
    testProperty "PhyloData" (jsonRoundtrip @PhyloData)
  ]
  ]

testWithQueryFrontend :: Assertion
testWithQueryFrontend = do
  assertBool "JSON instance will break frontend!"
    (isRight $ eitherDecode @WithQuery (C8.pack cannedWithQueryPayload))

-- The aim of this type is to catch regressions in the frontend serialisation; this
-- is what the frontend currently expects, and therefore if we were to change the JSON
-- instances, this test would fail, and we will be notified.
cannedWithQueryPayload :: String
cannedWithQueryPayload = [r| {"query":"Haskell","node_id":138,"lang":"EN","flowListWith":{"type":"MyListsFirst"},"datafield":"External Arxiv","databases":"Arxiv"} |]
