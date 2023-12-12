
module Test.API where

import Prelude
import Test.Hspec
import qualified Test.API.Authentication as Auth
import qualified Test.API.Errors as Errors
import qualified Test.API.GraphQL as GraphQL
import qualified Test.API.Private as Private
import qualified Test.API.UpdateList as UpdateList
import qualified Test.API.Phylo as Phylo

tests :: Spec
tests = describe "API" $ do
  Auth.tests
  Private.tests
  GraphQL.tests
  Errors.tests
  UpdateList.tests
  Phylo.tests
