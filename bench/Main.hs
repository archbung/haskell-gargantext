{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import Gargantext.Core.Types.Individu
import Gargantext.Prelude.Crypto.Auth (createPasswordHash)

import Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bgroup "Benchmarks"
    [ bgroup "User creation" [
      bench "createPasswordHash"  $ whnfIO (createPasswordHash "rabbit")
    , bench "toUserHash"  $
        whnfIO (toUserHash $ NewUser "alfredo" "alfredo@well-typed.com" (GargPassword "rabbit"))
    ]
    ]
  ]
