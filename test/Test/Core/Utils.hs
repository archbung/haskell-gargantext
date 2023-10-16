{-|
Module      : Core.Utils
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Test.Core.Utils where

import Gargantext.Core.Utils
import Gargantext.Prelude
import Test.Hspec

-- | Core.Utils tests
test :: Spec
test = do
  describe "check if groupWithCounts works" $ do
    it "simple integer array" $ do
      (groupWithCounts [1, 2, 3, 1, 2, 3]) `shouldBe` [(1, 2), (2, 2), (3, 2)]

    it "string" $ do
      (groupWithCounts "abccba") `shouldBe` [('a', 2), ('b', 2), ('c', 2)]
