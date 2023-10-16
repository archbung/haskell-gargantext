
{-|
Module      : Ngrams.NLP
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Test.Ngrams.NLP where

import Gargantext.Core.Text.Terms.Multi
import Gargantext.Prelude
import Test.Hspec


test :: Spec
test = do
  describe "Text that should be cleaned before sending it to NLP tools as micro-services." $ do
    let text   = "This is a url http://cnrs.gargantext.org to be remove and another one www.gargantext.org and digits 343242-2332 to be remove and some to keep: 232 231 33." :: Text
    let result = "This is a url to be remove and another one and digits to be remove and some to keep: 232 231 33."
    it "NLP Clean Text before sending to micro services:" $ cleanTextForNLP text `shouldBe` result

