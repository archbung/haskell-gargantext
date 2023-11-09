{-|
Module      : Main.hs
Description : Gargantext Import Corpus
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Import a corpus binary.

 -}

{-# LANGUAGE Strict            #-}

module Main where

import Gargantext.API.Dev (withDevEnv, runCmdDev)
import Gargantext.API.Errors.Types
import Gargantext.API.Node () -- instances only
import Gargantext.Core.Types.Individu (User(..), arbitraryNewUsers, NewUser(..), arbitraryUsername, GargPassword(..))
import Gargantext.Database.Action.Flow (getOrMkRoot, getOrMk_RootWithCorpus)
import Gargantext.Database.Admin.Config (userMaster, corpusMasterName)
import Gargantext.Database.Admin.Trigger.Init (initFirstTriggers, initLastTriggers)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataCorpus)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd, DBCmd)
import Gargantext.Database.Query.Table.Node (getOrMkList)
import Gargantext.Database.Query.Table.User (insertNewUsers, )
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig(..), readConfig)
import qualified Data.List.NonEmpty as NE


main :: IO ()
main = do
  params@[iniPath] <- getArgs

  _ <- if length params /= 1
      then panic "USAGE: ./gargantext-init gargantext.ini"
      else pure ()

  putStrLn ("Enter master user (gargantua) _password_ :" :: Text)
  password  <- getLine

  putStrLn ("Enter master user (gargantua) _email_ :" :: Text)
  email     <- getLine

  cfg       <- readConfig         iniPath
  let secret = _gc_secretkey cfg

  let createUsers :: Cmd BackendInternalError Int64
      createUsers = insertNewUsers (NewUser "gargantua" (cs email) (GargPassword $ cs password)
                                   NE.:| arbitraryNewUsers
                                   )

  let
    mkRoots :: Cmd BackendInternalError [(UserId, RootId)]
    mkRoots = mapM getOrMkRoot $ map UserName ("gargantua" : arbitraryUsername)
    -- TODO create all users roots

  let
    initMaster :: Cmd BackendInternalError (UserId, RootId, CorpusId, ListId)
    initMaster = do
      (masterUserId, masterRootId, masterCorpusId)
                  <- getOrMk_RootWithCorpus (UserName userMaster)
                                            (Left corpusMasterName)
                                            (Nothing :: Maybe HyperdataCorpus)
      masterListId <- getOrMkList masterCorpusId masterUserId
      _triggers    <- initLastTriggers masterListId
      pure (masterUserId, masterRootId, masterCorpusId, masterListId)

  withDevEnv iniPath $ \env -> do
    _ <- runCmdDev env (initFirstTriggers secret :: DBCmd BackendInternalError [Int64])
    _ <- runCmdDev env createUsers
    x <- runCmdDev env initMaster
    _ <- runCmdDev env mkRoots
    putStrLn (show x :: Text)
    pure ()
