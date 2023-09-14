{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Operations (
   tests
  ) where

import Control.Exception hiding (assert)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Pool hiding (withResource)
import Data.String
import Database.PostgreSQL.Simple
import Gargantext.API.Node.Corpus.Update
import Gargantext.Core
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.User
import Gargantext.Database.Action.User.New
import Gargantext.Database.Admin.Types.Hyperdata.Corpus
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node (mk, getCorporaWithParentId, getOrMkList)
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

import Database.Operations.Types
import Database.Operations.DocumentSearch
import Paths_gargantext
import Test.Hspec
import Test.QuickCheck.Monadic
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck
import Gargantext.Database.Admin.Trigger.Init
import Gargantext.Database.Action.Flow
import Gargantext.Database.Admin.Config (userMaster, corpusMasterName)

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
  describe "Prelude" $ do
    it "setup DB triggers" setupEnvironment
  describe "Read/Writes" $ do
    describe "User creation" $ do
      it "Simple write/read" writeRead01
      it "Simple duplicate"  mkUserDup
      it "Read/Write roundtrip" prop_userCreationRoundtrip
    describe "Corpus creation" $ do
      it "Simple write/read" corpusReadWrite01
      it "Can add language to Corpus" corpusAddLanguage
      it "Can add documents to a Corpus" corpusAddDocuments
    describe "Corpus search" $ do
      it "Can stem query terms" stemmingTest
      it "Can perform a simple search inside documents" corpusSearch01
      it "Can perform search by author in documents" corpusSearch02
      it "Can perform more complex searches using the boolean API" corpusSearch03

data ExpectedActual a =
    Expected a
  | Actual a
  deriving Show

instance Eq a => Eq (ExpectedActual a) where
  (Expected a) == (Actual b)   = a == b
  (Actual a)   == (Expected b) = a == b
  _ == _ = False

setupEnvironment :: TestEnv -> IO ()
setupEnvironment env = flip runReaderT env $ runTestMonad $ do
  void $ initFirstTriggers "secret_key"
  void $ new_user $ mkNewUser (userMaster <> "@cnrs.com") (GargPassword "secret_key")
  (masterUserId, _masterRootId, masterCorpusId)
              <- getOrMk_RootWithCorpus (UserName userMaster)
                                        (Left corpusMasterName)
                                        (Nothing :: Maybe HyperdataCorpus)
  masterListId <- getOrMkList masterCorpusId masterUserId
  void $ initLastTriggers masterListId

writeRead01 :: TestEnv -> Assertion
writeRead01 env = do
  flip runReaderT env $ runTestMonad $ do
    let nur1 = mkNewUser "alfredo@well-typed.com" (GargPassword "my_secret")
    let nur2 = mkNewUser "paul@acme.com" (GargPassword "my_secret")

    uid1 <- new_user nur1
    uid2 <- new_user nur2

    liftBase $ uid1 `shouldBe` 2
    liftBase $ uid2 `shouldBe` 3

    -- Getting the users by username returns the expected IDs
    uid1' <- getUserId (UserName "alfredo")
    uid2' <- getUserId (UserName "paul")
    liftBase $ uid1' `shouldBe` 2
    liftBase $ uid2' `shouldBe` 3

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

-- | We test that we can create and later read-back a 'Corpus'.
corpusReadWrite01 :: TestEnv -> Assertion
corpusReadWrite01 env = do
  flip runReaderT env $ runTestMonad $ do
    uid      <- getUserId (UserName "alfredo")
    parentId <- getRootId (UserName "alfredo")
    [corpusId] <- mk (Just "Test_Corpus") (Nothing :: Maybe HyperdataCorpus) parentId uid
    liftIO $ corpusId `shouldBe` NodeId 416
    -- Retrieve the corpus by Id
    [corpus] <- getCorporaWithParentId parentId
    liftIO $ corpusId `shouldBe` (_node_id corpus)

-- | We test that we can update the existing language for a 'Corpus'.
corpusAddLanguage :: TestEnv -> Assertion
corpusAddLanguage env = do
  flip runReaderT env $ runTestMonad $ do
    parentId <- getRootId (UserName "alfredo")
    [corpus] <- getCorporaWithParentId parentId
    liftIO $ (_hc_lang . _node_hyperdata $ corpus) `shouldBe` Just EN -- defaults to English
    addLanguageToCorpus (_node_id corpus) IT
    [corpus'] <- getCorporaWithParentId parentId
    liftIO $ (_hc_lang . _node_hyperdata $ corpus') `shouldBe` Just IT
