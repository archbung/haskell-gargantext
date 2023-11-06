{-|
Module      : Main.hs
Description : Gargantext Admin tools
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX



 -}

{-# LANGUAGE Strict            #-}

module Main where

import Gargantext.API.Dev (withDevEnv, runCmdDev)
import Gargantext.API.Errors.Types
import Gargantext.Database.Action.User.New (newUsers)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd'')
import Gargantext.Prelude
import Gargantext.API.Admin.EnvTypes (DevEnv)
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = do
  (iniPath:mails) <- getArgs

  withDevEnv iniPath $ \env -> do
    x <- runCmdDev env ((newUsers $ NE.map cs (NE.fromList mails)) :: Cmd'' DevEnv BackendInternalError (NonEmpty UserId))
    putStrLn (show x :: Text)
  pure ()
