{-# LANGUAGE ScopedTypeVariables #-}
module Test.Utils where

import Prelude
import Control.Exception
import Test.Tasty.HUnit

-- | Marks the input 'Assertion' as pending, by ignoring any exception
-- thrown by it.
pending :: String -> Assertion -> Assertion
pending reason act = act `catch` (\(e :: SomeException) -> do
  putStrLn $ "PENDING: " <> reason
  putStrLn (displayException e))
