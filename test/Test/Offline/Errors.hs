{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Offline.Errors (tests) where

import Control.Exception
import Gargantext.Core (fromDBid)
import Gargantext.Core.Errors.Types
import Gargantext.Database.Admin.Config ()
import Gargantext.Database.Admin.Types.Node
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Errors" [
    testCase "fromDBid comes with a CallStack" fromDBid_cs
  ]

fromDBid_cs :: Assertion
fromDBid_cs = do
  res <- try $ evaluate $ fromDBid @NodeType 99
  case res of
    Right r -> fail $ "fromDBid should have failed, but returned: " <> show r
    Left (_ :: WithStacktrace IOError)
      -> pure ()
