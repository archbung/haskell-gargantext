
module Main where

import Gargantext.Prelude

import qualified Database.Operations     as DB

import Test.Hspec

-- It's especially important to use Hspec for DB tests, because,
-- unlike 'tasty', 'Hspec' has explicit control over parallelism,
-- and it's important that DB tests are run according to a very
-- precise order, as they are not independent from each other.
-- Unfortunately it's not possibly to use the 'tasty-hspec' adapter
-- because by the time we get a 'TestTree' out of the adapter library,
-- the information about parallelism is lost.
main :: IO ()
main = hspec DB.tests
