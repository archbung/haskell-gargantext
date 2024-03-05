{-|
Module      : Core.Similarity
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Test.Core.Similarity where

import Gargantext.Core.Methods.Similarities.Conditional
import Gargantext.Prelude
import Test.Hspec

test :: Spec
test = do
  describe "check if similarities optimizations are well implemented" $ do
    it "Conditional" $ do
      conditional_test `shouldBe` True
