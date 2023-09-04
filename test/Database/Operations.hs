{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Database.Operations (
   tests
  ) where

import Control.Exception hiding (assert)
import Control.Lens hiding (elements)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.IORef
import Data.Pool hiding (withResource)
import Data.String
import Database.PostgreSQL.Simple
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.User
import Gargantext.Database.Action.User.New
import Gargantext.Database.Admin.Types.Hyperdata.Corpus
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node (mk, getCorporaWithParentId)
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Query.Tree.Root (getRootId)
import Gargantext.Database.Schema.Node (NodePoly(..))
import Gargantext.Prelude
import Gargantext.Prelude.Config
import Prelude
import Shelly hiding (FilePath, run)
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Options as Client
import qualified Database.Postgres.Temp as Tmp
import qualified Shelly as SH

import Paths_gargantext
import Test.Hspec
import Test.QuickCheck.Monadic
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck

-- | Keeps a log of usernames we have already generated, so that our
-- roundtrip tests won't fail.
uniqueArbitraryNewUser :: Int -> Gen (NewUser GargPassword)
uniqueArbitraryNewUser currentIx = do
  ur <- (`mappend` (T.pack (show currentIx) <> "-")) <$> ascii_txt
  let email = ur <> "@foo.com"
  NewUser <$> pure ur <*> pure email <*> elements arbitraryPassword
  where
   ascii_txt :: Gen T.Text
   ascii_txt = fmap (T.pack . getPrintableString) arbitrary

-- | Test DB settings.
dbUser, dbPassword, dbName :: String
dbUser = "gargantua"
dbPassword = "gargantua_test"
dbName = "gargandb_test"

newtype Counter = Counter { _Counter :: IORef Int }
  deriving Eq

instance Show Counter where
  show (Counter _) = "Counter"

emptyCounter :: IO Counter
emptyCounter = Counter <$> newIORef 0

nextCounter :: Counter -> IO Int
nextCounter (Counter ref) = atomicModifyIORef' ref (\old -> (succ old, old))

data TestEnv = TestEnv {
    test_db          :: !DBHandle
  , test_config      :: !GargConfig
  , test_usernameGen :: !Counter
  }

newtype TestMonad a = TestMonad { runTestMonad :: ReaderT TestEnv IO a }
  deriving ( Functor, Applicative, Monad
           , MonadReader TestEnv, MonadError IOException
           , MonadBase IO
           , MonadBaseControl IO
           , MonadFail
           , MonadIO
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
      pure $ TestEnv (DBHandle pool db) gargConfig ugen

withTestDB :: (TestEnv -> IO ()) -> IO ()
withTestDB = bracket setup teardown

tests :: Spec
tests = sequential $ aroundAll withTestDB $ describe "Database" $ do
  describe "Read/Writes" $ do
    describe "User creation" $ do
      it "Simple write/read" writeRead01
      it "Simple duplicate"  mkUserDup
      it "Read/Write roundtrip" prop_userCreationRoundtrip
    describe "Corpus creation" $ do
      it "Simple write/read" corpusReadWrite01

data ExpectedActual a =
    Expected a
  | Actual a
  deriving Show

instance Eq a => Eq (ExpectedActual a) where
  (Expected a) == (Actual b)   = a == b
  (Actual a)   == (Expected b) = a == b
  _ == _ = False


writeRead01 :: TestEnv -> Assertion
writeRead01 env = do
  flip runReaderT env $ runTestMonad $ do
    let nur1 = mkNewUser "alfredo@well-typed.com" (GargPassword "my_secret")
    let nur2 = mkNewUser "paul@acme.com" (GargPassword "my_secret")

    uid1 <- new_user nur1
    uid2 <- new_user nur2

    liftBase $ uid1 `shouldBe` 1
    liftBase $ uid2 `shouldBe` 2

    -- Getting the users by username returns the expected IDs
    uid1' <- getUserId (UserName "alfredo")
    uid2' <- getUserId (UserName "paul")
    liftBase $ uid1' `shouldBe` 1
    liftBase $ uid2' `shouldBe` 2

mkUserDup :: TestEnv -> Assertion
mkUserDup env = do
  let x = flip runReaderT env $ runTestMonad $ do
            -- This should fail, because user 'alfredo' exists already.
            let nur = mkNewUser "alfredo@well-typed.com" (GargPassword "my_secret")
            new_user nur
  --
  -- SqlError {sqlState = "23505", sqlExecStatus = FatalError
  --          , sqlErrorMsg = "duplicate key value violates unique constraint \"auth_user_username_idx1\""
  --          , sqlErrorDetail = "Key (username)=(alfredo) already exists.", sqlErrorHint = ""
  --          }
  --
  -- Postgres increments the underlying SERIAL for the user even if the request fails, see
  -- https://stackoverflow.com/questions/37204749/serial-in-postgres-is-being-increased-even-though-i-added-on-conflict-do-nothing
  -- This means that the next available ID is '3'.
  x `shouldThrow` (\SqlError{..} -> sqlErrorDetail == "Key (username)=(alfredo) already exists.")

runEnv :: TestEnv -> TestMonad a -> PropertyM IO a
runEnv env act = run (flip runReaderT env $ runTestMonad act)

prop_userCreationRoundtrip :: TestEnv -> Property
prop_userCreationRoundtrip env = monadicIO $ do
  nextAvailableCounter <- run (nextCounter $ test_usernameGen env)
  nur  <- pick (uniqueArbitraryNewUser nextAvailableCounter)
  uid <- runEnv env (new_user nur)
  ur' <- runEnv env (getUserId (UserName $ _nu_username nur))
  run (Expected uid `shouldBe` Actual ur')

corpusReadWrite01 :: TestEnv -> Assertion
corpusReadWrite01 env = do
  flip runReaderT env $ runTestMonad $ do
    uid      <- getUserId (UserName "alfredo")
    parentId <- getRootId (UserName "alfredo")
    [corpusId] <- mk (Just "Test_Corpus") (Nothing :: Maybe HyperdataCorpus) parentId uid
    liftIO $ corpusId `shouldBe` NodeId 409
    -- Retrieve the corpus by Id
    [corpusId'] <- getCorporaWithParentId parentId
    liftIO $ corpusId `shouldBe` (_node_id corpusId')
