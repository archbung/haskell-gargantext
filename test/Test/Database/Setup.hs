{-# LANGUAGE TupleSections #-}
module Test.Database.Setup (
    withTestDB
  , fakeIniPath
  , testEnvToPgConnectionInfo
  ) where

import Control.Exception hiding (assert)
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Pool hiding (withResource)
import Data.Pool qualified as Pool
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Options qualified as Client
import Database.PostgreSQL.Simple.Options qualified as Opts
import Database.Postgres.Temp qualified as Tmp
import Gargantext.API.Admin.EnvTypes (Mode(Mock))
import Gargantext.Prelude.Config
import Gargantext.System.Logging (withLoggerHoisted)
import Paths_gargantext
import Prelude
import Shelly hiding (FilePath, run)
import Shelly qualified as SH
import Test.Database.Types

-- | Test DB settings.
dbUser, dbPassword, dbName :: String
dbUser = "gargantua"
dbPassword = "gargantua_test"
dbName = "gargandb_test"

fakeIniPath :: IO FilePath
fakeIniPath = getDataFileName "test-data/test_config.ini"

gargDBSchema :: IO FilePath
gargDBSchema = getDataFileName "devops/postgres/schema.sql"

teardown :: TestEnv -> IO ()
teardown TestEnv{..} = do
  destroyAllResources $ _DBHandle test_db
  Tmp.stop $ _DBTmp test_db

-- | Bootstraps the DB, by creating the DB and the schema.
bootstrapDB :: Tmp.DB -> Pool PG.Connection -> GargConfig -> IO ()
bootstrapDB tmpDB pool _cfg = Pool.withResource pool $ \conn -> do
  void $ PG.execute_ conn (fromString $ "ALTER USER \"" <> dbUser <> "\" with PASSWORD '" <> dbPassword <> "'")
  schemaPath <- gargDBSchema
  let connString = Tmp.toConnectionString tmpDB
  (res,ec) <- shelly $ silently $ escaping False $ do
    result <- SH.run "psql" ["-d", "\"" <> TE.decodeUtf8 connString <> "\"", "<", fromString schemaPath]
    (result,) <$> lastExitCode
  unless (ec == 0) $ throwIO (userError $ show ec <> ": " <> T.unpack res)

tmpPgConfig :: Tmp.Config
tmpPgConfig = Tmp.defaultConfig <>
  Tmp.optionsToDefaultConfig mempty
    { Client.dbname   = pure dbName
    , Client.user     = pure dbUser
    , Client.password = pure dbPassword
    }

setup :: IO TestEnv
setup = do
  res <- Tmp.startConfig tmpPgConfig
  case res of
    Left err -> fail $ show err
    Right db -> do
      gargConfig <- fakeIniPath >>= readConfig
      pool <- createPool (PG.connectPostgreSQL (Tmp.toConnectionString db))
                         (PG.close) 2 60 2
      bootstrapDB db pool gargConfig
      ugen <- emptyCounter
      withLoggerHoisted Mock $ \logger -> do
        pure $ TestEnv (DBHandle pool db) gargConfig ugen logger

withTestDB :: (TestEnv -> IO ()) -> IO ()
withTestDB = bracket setup teardown

testEnvToPgConnectionInfo :: TestEnv -> PG.ConnectInfo
testEnvToPgConnectionInfo TestEnv{..} =
  PG.ConnectInfo { PG.connectHost     = "0.0.0.0"
                 , PG.connectPort     = fromIntegral $ fromMaybe 5432
                                                     $ getLast
                                                     $ Opts.port
                                                     $ Tmp.toConnectionOptions
                                                     $ _DBTmp test_db
                 , PG.connectUser     = dbUser
                 , PG.connectPassword = dbPassword
                 , PG.connectDatabase = dbName
                 }
