
module Test.API where

import Prelude
import Test.Hspec
import qualified Test.API.Authentication as Auth

tests :: Spec
tests = describe "API" $
  Auth.tests
