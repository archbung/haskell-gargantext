{-|
Module      : Main.hs
Description : Gargantext DB obfuscation
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

See issue
https://gitlab.iscpif.fr/gargantext/haskell-gargantext/issues/282


This script obfuscates the DB. We don't use gargantext.ini on purpose,
so that we don't accidentally break a running DB.

The procedure is that you clone the DB and provide the cloned DB
location to this script.

Copy the DB with this SQL statement:

CREATE DATABASE "gargantext_copy" WITH TEMPLATE "gargandbV5" OWNER gargantua;

https://stackoverflow.com/questions/876522/creating-a-copy-of-a-database-in-postgresql

 -}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Strict            #-}

module Main where


import Data.Text qualified as T
import Database.PostgreSQL.Simple qualified as PSQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Core (toDBid)
import Gargantext.Database.Admin.Config ()
import Gargantext.Database.Admin.Types.Node (NodeType(..))
import Gargantext.Prelude hiding (option)
import Gargantext.Prelude.Database (runPGSExecute, runPGSQuery)
import Options.Applicative.Simple



data Args = Args {
    dbHost     :: Text
  , dbPort     :: Int
  , dbName     :: Text
  , dbUser     :: Text
  , dbPassword :: Text
  } deriving (Show, Eq)


args :: Parser Args
args = Args
  <$> ( strOption ( long "db-host"
                    <> metavar "db-host"
                    <> help "Location of the DB server"
                    <> value "localhost"
                    <> showDefault ) )
  <*> ( option auto ( long "db-port"
                      <> metavar "db-port"
                      <> value 5432 ) )
  <*> ( strOption ( long "db-name"
                    <> metavar "db-name"
                    <> value "gargantext_copy" ) )
  <*> ( strOption ( long "db-user"
                    <> metavar "db-user"
                    <> value "gargantua" ) )
  <*> ( strOption ( long "db-password"
                    <> metavar "db-password"
                    <> value "" ))
  

main :: IO ()
main = do
  (opts, ()) <-
    simpleOptions "0.0.1"
                  "gargantext DB obfuscation"
                  "Obfuscates a cloned Gargantext DB"
                  args
                  empty

  putText $ show opts

  let ci = PSQL.ConnectInfo { connectHost     = T.unpack $ dbHost opts
                            , connectPort     = fromIntegral $ dbPort opts
                            , connectUser     = T.unpack $ dbUser opts
                            , connectPassword = T.unpack $ dbPassword opts
                            , connectDatabase = T.unpack $ dbName opts }

  putText $ show ci

  c <- PSQL.connect ci

  obfuscateNotes c



obfuscateNotes :: PSQL.Connection -> IO ()
obfuscateNotes c = do
  let nt = toDBid Notes
  
  _ <- runPGSExecute c [sql|UPDATE nodes SET name = concat('notes-', id) WHERE typename = ?;|] (PSQL.Only nt)

  nsNew <- runPGSQuery c [sql|SELECT id, name FROM nodes WHERE typename = ?|] (PSQL.Only nt) :: IO [(Int, Text)]
  putText $ show nsNew

  _ <- runPGSExecute c [sql|UPDATE nodes SET hyperdata = jsonb_set(hyperdata, '{frame_id}', '"xxx"', false) WHERE typename = ?;|] (PSQL.Only nt)

  frameIdsNew <- runPGSQuery c [sql|SELECT id, hyperdata ->> 'frame_id' FROM nodes WHERE typename = ?;|] (PSQL.Only nt) :: IO [(Int, Text)]
  putText $ show frameIdsNew

  pure ()
