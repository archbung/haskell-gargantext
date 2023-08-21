{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Database.Operations where

import Control.Exception hiding (assert)
import Control.Lens hiding (elements)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.IORef
import Data.Pool hiding (withResource)
import Data.String
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.User hiding (Username)
import Gargantext.Database.Action.User.New
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Schema.User
import Gargantext.Prelude
import Gargantext.Prelude.Config
import Prelude
import Shelly hiding (FilePath, run)
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import qualified Data.Pool as Pool
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Options as Client
import qualified Database.Postgres.Temp as Tmp
import qualified Shelly as SH

import Paths_gargantext

uniqueArbitraryNewUser :: S.Set Username -> Gen (NewUser GargPassword)
uniqueArbitraryNewUser alreadyTakenNames = do
  ur <- ascii_txt `suchThat` (not . flip S.member alreadyTakenNames)
  NewUser <$> pure ur <*> ascii_txt <*> elements arbitraryPassword
  where
   ascii_txt :: Gen T.Text
   ascii_txt = fmap (T.pack . getPrintableString) arbitrary

-- | Test DB settings.
dbUser, dbPassword, dbName :: String
dbUser = "gargantua"
dbPassword = "gargantua_test"
dbName = "gargandbV5"


data TestEnv = TestEnv {
    test_db          :: !DBHandle
  , test_config      :: !GargConfig
  , test_usernameGen :: !(IORef (S.Set Username))
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
      ugen <- newIORef mempty
      pure $ TestEnv (DBHandle pool db) gargConfig ugen

tests :: TestTree
tests = withResource setup teardown $
  \getEnv -> testGroup "Database" [unitTests getEnv]

unitTests :: IO TestEnv -> TestTree
unitTests getEnv = testGroup "Read/Writes"
  [ testGroup "User creation" [
      testCase     "Simple write" (write01 getEnv)
    , testProperty "Read/Write roundtrip" $ withMaxSuccess 50 (prop_userCreationRoundtrip getEnv)
    ]
  ]

write01 :: IO TestEnv -> Assertion
write01 getEnv = do
  env <- getEnv
  flip runReaderT env $ runTestMonad $ do
    let nur = mkNewUser "alfredo@well-typed.com" (GargPassword "my_secret")
    x <- new_user nur
    liftBase $ x `shouldBe` 1

runEnv :: TestEnv -> TestMonad a -> PropertyM IO a
runEnv env act = run (flip runReaderT env $ runTestMonad act)

prop_userCreationRoundtrip :: IO TestEnv -> Property
prop_userCreationRoundtrip getEnv = monadicIO $ do
  env  <- run getEnv
  alreadyTakenUsernames <- run (readIORef $ test_usernameGen env)
  nur  <- pick (uniqueArbitraryNewUser alreadyTakenUsernames)
  void $ runEnv env (new_user nur)
  ur' <- runEnv env (getUserLightDB (UserName $ _nu_username nur))
  assert (userLight_username ur' == _nu_username nur)
  assert (userLight_email ur'    == _nu_email nur)
  run (writeIORef (test_usernameGen env) $ S.insert (_nu_username nur) alreadyTakenUsernames)
