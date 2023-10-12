{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Test.Database.Operations (
   tests
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text qualified as T
import Database.PostgreSQL.Simple
import Gargantext.API.Node.Corpus.Update
import Gargantext.Core
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.User
import Gargantext.Database.Action.User.New
import Gargantext.Database.Admin.Types.Hyperdata.Corpus
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Tree.Root (getRootId)
import Gargantext.Database.Schema.Node (NodePoly(..))
import Gargantext.Prelude
import Test.API.Setup (setupEnvironment)
import Test.Database.Operations.DocumentSearch
import Test.Database.Setup (withTestDB)
import Test.Database.Types
import Test.Hspec
import Test.QuickCheck.Monadic
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck

-- | Keeps a log of usernames we have already generated, so that our
-- roundtrip tests won't fail.
uniqueArbitraryNewUser :: Int -> Gen (NewUser GargPassword)
uniqueArbitraryNewUser currentIx = do
  ur <- (`mappend` ((show currentIx :: Text) <> "-")) <$> ascii_txt
  let email = ur <> "@foo.com"
  NewUser <$> pure ur <*> pure email <*> elements arbitraryPassword
  where
   ascii_txt :: Gen T.Text
   ascii_txt = fmap (T.pack . getPrintableString) arbitrary

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
      it "Can correctly count doc score" corpusScore01

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
