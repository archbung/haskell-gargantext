{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Database.Operations where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Pool hiding (withResource)
import Gargantext.Database.Action.User.New
import Gargantext.Database.Prelude
import Gargantext.Prelude
import Gargantext.Prelude.Config
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Postgres.Temp as Tmp

import Paths_gargantext
import Control.Lens
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Core.Types.Individu

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

teardown :: TestEnv -> IO ()
teardown TestEnv{..} = do
  destroyAllResources $ _DBHandle test_db
  Tmp.stop $ _DBTmp test_db

setup :: IO TestEnv
setup = do
  res <- Tmp.startConfig Tmp.defaultConfig
  case res of
    Left err -> fail $ show err
    Right db -> do
      pool <- createPool (PG.connectPostgreSQL (Tmp.toConnectionString db))
                         (PG.close)
                         2
                         60
                         2
      TestEnv <$> (pure $ DBHandle pool db) <*> (fakeIniPath >>= readConfig)

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