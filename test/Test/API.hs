
module Test.API where

import Prelude
import Test.Hspec
import qualified Test.API.Authentication as Auth
import qualified Test.API.Private as Private

tests :: Spec
tests = describe "API" $ do
  Auth.tests
  Private.tests
