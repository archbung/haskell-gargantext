
module Test.API where

import Prelude
import Test.Hspec
import qualified Test.API.Authentication as Auth
import qualified Test.API.Private as Private
import qualified Test.API.GraphQL as GraphQL

tests :: Spec
tests = describe "API" $ do
  Auth.tests
  Private.tests
  GraphQL.tests
