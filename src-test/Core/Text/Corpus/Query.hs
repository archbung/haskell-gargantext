{-# LANGUAGE OverloadedStrings #-}
module Core.Text.Corpus.Query (tests) where

import Data.BoolExpr
import Gargantext.Core.Text.Corpus.Query
import Prelude

import Test.Tasty
import Test.Tasty.QuickCheck hiding (Positive)

tests :: TestTree
tests = testGroup "Boolean Query Engine" [
  testProperty "Parses 'A OR B'" testParse01
  ]

testParse01 :: Property
testParse01 =
  (renderQuery <$> parseQuery "A OR B") === (renderQuery <$> Right (unsafeMkQuery $ (BConst (Positive "A") `BOr` BConst (Positive "B"))))
