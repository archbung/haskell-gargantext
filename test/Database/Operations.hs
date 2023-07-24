{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Database.Operations where

import Control.Exception
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Pool hiding (withResource)
import Data.String
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.User.New
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Prelude
import Gargantext.Prelude.Config
import Prelude
import Shelly hiding (FilePath)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Options as Client
import qualified Database.Postgres.Temp as Tmp

import Paths_gargantext


-- | Test DB settings.
dbUser, dbPassword, dbName :: String
dbUser = "gargantua"
dbPassword = "gargantua_test"
dbName = "gargandbV5"


data TestEnv = TestEnv {
    test_db     :: !DBHandle
  , test_config :: !GargConfig
  }

newtype TestMonad a = TestMonad { runTestMonad :: ReaderT TestEnv IO a }
  deriving ( Functor, Applicative, Monad
           , MonadReader TestEnv, MonadError IOException
           , MonadBase IO
           , MonadBaseControl IO
           )

data DBHandle = DBHandle { 
    _DBHandle :: Pool PG.Connection
  , _DBTmp    :: Tmp.DB
  }

instance HasNodeError IOException where
  _NodeError = prism' (userError . show) (const Nothing)

instance HasConnectionPool TestEnv where
  connPool = to (_DBHandle . test_db)

instance HasConfig TestEnv where
  hasConfig = to test_config

fakeIniPath :: IO FilePath
fakeIniPath = getDataFileName "test-data/test_config.ini"

gargDBSchema :: IO FilePath
gargDBSchema = getDataFileName "devops/postgres/schema.sql"

gargDBExtensionsSchema :: IO FilePath
gargDBExtensionsSchema = getDataFileName "devops/postgres/extensions.sql"

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
    result <- run "psql" ["-d", "\"" <> TE.decodeUtf8 connString <> "\"", "<", fromString schemaPath]
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
                         (PG.close)
                         2
                         60
                         2
      bootstrapDB db pool gargConfig
      pure $ TestEnv (DBHandle pool db) gargConfig

tests :: TestTree
tests = withResource setup teardown $
  \getEnv -> testGroup "Database" [unitTests getEnv]

unitTests :: IO TestEnv -> TestTree
unitTests getEnv = testGroup "Read/Writes"
  [ testCase "Simple write" (write01 getEnv)
  ]

write01 :: IO TestEnv -> Assertion
write01 getEnv = do
  env <- getEnv
  flip runReaderT env $ runTestMonad $ do
    let nur = mkNewUser "alfredo@well-typed.com" (GargPassword "my_secret")
    x <- new_user nur
    liftBase $ x `shouldBe` 1