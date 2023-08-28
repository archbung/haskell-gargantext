{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Core.Text.Corpus.Query (tests) where

import Data.BoolExpr
import Data.Conduit
import Data.String
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Query
import Gargantext.Core.Types
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Prelude
import System.Environment
import qualified Data.Text as T
import qualified Gargantext.Core.Text.Corpus.API.Arxiv as Arxiv
import qualified Gargantext.Core.Text.Corpus.API.Pubmed as Pubmed
import qualified Network.Api.Arxiv as Arxiv

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Positive, Negative)

newtype PubmedApiKey
  = PubmedApiKey { _PubmedApiKey :: T.Text }
  deriving stock (Show, Eq)
  deriving newtype IsString

pubmedSettings :: IO (Maybe PubmedApiKey)
pubmedSettings = fmap fromString <$> lookupEnv "PUBMED_API_KEY"

tests :: TestTree
tests = withResource pubmedSettings (const (pure ())) $ \getPubmedKey ->
  testGroup "Boolean Query Engine" [
    testProperty "Parses 'A OR B'" testParse01
  , testProperty "Parses 'A AND B'" testParse02
  , testProperty "Parses '-A'" testParse03
  , testProperty "Parses 'NOT A'" testParse03_01
  , testProperty "Parses 'A -B'" testParse04
  , testProperty "Parses 'A NOT -B'" testParse04_01
  , testProperty "Parses 'A AND B -C'   (left associative)" testParse05
  , testProperty "Parses 'A AND (B -C)' (right associative)" testParse05_01
  , testProperty "Parses (A OR B OR NOT C) AND (D OR E OR F) -(G OR H OR I)" testParse06
  , testCase     "Parses words into a single constant" testWordsIntoConst
  , testGroup "Arxiv expression converter" [
      testCase "It supports 'A AND B'"     testArxiv01_01
    , testCase "It supports '\"Haskell\" AND \"Agda\"'" testArxiv01_02
    , testCase "It supports 'A OR B'"        testArxiv02
    , testCase "It supports 'A AND NOT B'"   testArxiv03_01
    , testCase "It supports 'A AND -B'"      testArxiv03_02
    , testCase "It supports 'A AND -B'"      testArxiv03_02
    , testCase "It supports 'A AND NOT (NOT B)'"  testArxiv04_01
    , testCase "It supports 'A AND NOT (NOT (NOT B))'"  testArxiv04_02
    , testCase "It supports 'A OR NOT B'"  testArxiv05
    , testCase "It supports '-A'"  testArxiv06
    ]
  , testGroup "PUBMED expression converter" [
      testCase "It supports 'A'" testPubMed01
    , testCase "It supports '-A'" testPubMed02_01
    , testCase "It supports 'NOT A'" testPubMed02_02
    , testCase "It supports 'NOT (NOT A)'" testPubMed02_03
    , testCase "It supports '\"Haskell\" AND \"Idris\"'" testPubMed03
    , testCase "It supports 'A OR B'" testPubMed04
    ]
  , testGroup "PUBMED real queries (skipped if PUBMED_API_KEY env var not set)" [
      testCase "It searches for \"Covid\"" (testPubMedCovid_01 getPubmedKey)
    , testCase "It searches for \"Covid\" AND \"Alzheimer\"" (testPubMedCovid_02 getPubmedKey)
    ]
  -- We skip the Arxiv tests if the PUBMED_API_KEY is not set just for conveniency, to have
  -- only a single flow-control mechanism.
  , testGroup "ARXIV real queries (skipped if PUBMED_API_KEY env var not set)" [
      testCase "It searches for \"Haskell\"" (testArxivRealWorld_01 getPubmedKey)
    , testCase "It searches for \"Haskell\" AND \"Agda\"" (testArxivRealWorld_02 getPubmedKey)
    ]
  ]

-- | Checks that the 'RawQuery' can be translated into the expected 'BoolExpr' form,
-- by also checking that both renders back to the initial 'RawQuery'.
translatesInto :: RawQuery -> BoolExpr Term -> Property
(translatesInto) raw boolExpr =
  let parsed   = parseQuery raw
      expected = Right (unsafeMkQuery boolExpr)
  in counterexample (show parsed <> " != " <> show expected) $
       (renderQuery <$> parsed) === (renderQuery <$> expected)

testParse01 :: Property
testParse01 = "A OR B" `translatesInto` (BConst (Positive "A") `BOr` BConst (Positive "B"))

testParse02 :: Property
testParse02 = "A AND B" `translatesInto` (BConst (Positive "A") `BAnd` BConst (Positive "B"))

testParse03 :: Property
testParse03 = "-A" `translatesInto` (BConst (Negative "A"))

testParse03_01 :: Property
testParse03_01 = "NOT A" `translatesInto` (BConst (Negative "A"))

testParse04 :: Property
testParse04 = "A -B" `translatesInto` (BConst (Positive "A") `BAnd` BConst (Negative "B"))

-- Both 'A -B' and 'A AND -B' desugars into the same form.
testParse04_01 :: Property
testParse04_01 = "A AND -B" `translatesInto` (BConst (Positive "A") `BAnd` BConst (Negative "B"))

testParse05 :: Property
testParse05 = "A AND B -C" `translatesInto` ((BConst (Positive "A") `BAnd` BConst (Positive "B")) `BAnd` BConst (Negative "C"))

testParse05_01 :: Property
testParse05_01 =
  "A AND (B -C)" `translatesInto` (BConst (Positive "A") `BAnd` (BConst (Positive "B") `BAnd` BConst (Negative "C")))

testParse06 :: Property
testParse06 =
  translatesInto "(A OR B OR NOT C) AND (D OR E OR F) -(G OR H OR I)"
    (
      (
        ((BConst (Positive  "A") `BOr` (BConst (Positive  "B"))) `BOr` (BConst  (Negative "C")))
        `BAnd`
        ((BConst (Positive "D") `BOr` (BConst (Positive "E"))) `BOr` (BConst (Positive "F")))
      )
      `BAnd` BNot (
        ((BConst (Positive "G") `BOr` (BConst (Positive "H"))) `BOr` (BConst (Positive "I")))
      )
    )

testWordsIntoConst :: Assertion
testWordsIntoConst =
  let (expected :: BoolExpr Term) =
        fromCNF (boolTreeToCNF @Term $ (BConst (Positive "The Art of Computer Programming") `BAnd` (BConst (Positive "Conceptual Mathematics"))))
  in case parseQuery "\"The Art of Computer Programming\" AND \"Conceptual Mathematics\"" of
    Left err
      -> assertBool err False
    Right x
      -> fromCNF (getQuery x) @?= expected

withValidQuery :: RawQuery -> (Query -> Assertion) -> Assertion
withValidQuery rawQuery onValidParse = do
  case parseQuery rawQuery of
    Left err -> assertBool err False
    Right x  -> onValidParse x


testArxiv01_01 :: Assertion
testArxiv01_01 = withValidQuery "A AND B" $ \q ->
  assertBool ("Query not converted into expression: " <> show @(BoolExpr Term) (fromCNF $ getQuery q))
             (Arxiv.qExp (Arxiv.convertQuery q) == Just (Arxiv.And (Arxiv.Exp $ Arxiv.Abs ["A"]) ((Arxiv.Exp $ Arxiv.Abs ["B"]))))

testArxiv01_02 :: Assertion
testArxiv01_02 = withValidQuery "\"Haskell\" AND \"Agda\"" $ \q ->
  assertBool ("Query not converted into expression: " <> show @(BoolExpr Term) (fromCNF $ getQuery q))
             (Arxiv.qExp (Arxiv.convertQuery q) == Just (Arxiv.And (Arxiv.Exp $ Arxiv.Abs ["Haskell"]) ((Arxiv.Exp $ Arxiv.Abs ["Agda"]))))

testArxiv02 :: Assertion
testArxiv02 = withValidQuery "A OR B" $ \q ->
  assertBool ("Query not converted into expression: " <> show @(BoolExpr Term) (fromCNF $ getQuery q))
             (Arxiv.qExp (Arxiv.convertQuery q) == Just (Arxiv.Or (Arxiv.Exp $ Arxiv.Abs ["A"]) ((Arxiv.Exp $ Arxiv.Abs ["B"]))))

testArxiv03_01 :: Assertion
testArxiv03_01 = withValidQuery "A AND NOT B" $ \q ->
  assertBool ("Query not converted into expression: " <> show @(BoolExpr Term) (fromCNF $ getQuery q))
             (Arxiv.qExp (Arxiv.convertQuery q) == Just (Arxiv.AndNot (Arxiv.Exp $ Arxiv.Abs ["A"]) ((Arxiv.Exp $ Arxiv.Abs ["B"]))))

testArxiv03_02 :: Assertion
testArxiv03_02 = withValidQuery "A AND -B" $ \q ->
  assertBool ("Query not converted into expression: " <> show @(BoolExpr Term) (fromCNF $ getQuery q))
             (Arxiv.qExp (Arxiv.convertQuery q) == Just (Arxiv.AndNot (Arxiv.Exp $ Arxiv.Abs ["A"]) ((Arxiv.Exp $ Arxiv.Abs ["B"]))))

-- Double negation get turned into positive.
testArxiv04_01 :: Assertion
testArxiv04_01 = withValidQuery "A AND NOT (NOT B)" $ \q ->
  assertBool ("Query not converted into expression: " <> show @(BoolExpr Term) (fromCNF $ getQuery q))
             (Arxiv.qExp (Arxiv.convertQuery q) == Just (Arxiv.And (Arxiv.Exp $ Arxiv.Abs ["A"]) ((Arxiv.Exp $ Arxiv.Abs ["B"]))))

testArxiv04_02 :: Assertion
testArxiv04_02 = withValidQuery "A AND NOT (NOT (NOT B))" $ \q ->
  assertBool ("Query not converted into expression: " <> show @(BoolExpr Term) (fromCNF $ getQuery q))
             (Arxiv.qExp (Arxiv.convertQuery q) == Just (Arxiv.AndNot (Arxiv.Exp $ Arxiv.Abs ["A"]) ((Arxiv.Exp $ Arxiv.Abs ["B"]))))

testArxiv05 :: Assertion
testArxiv05 = withValidQuery "A OR NOT B" $ \q ->
  assertBool ("Query not converted into expression: " <> show @(BoolExpr Term) (fromCNF $ getQuery q))
             (Arxiv.qExp (Arxiv.convertQuery q) == Just (
                Arxiv.Or (Arxiv.Exp $ Arxiv.Abs ["A"])
                         (Arxiv.AndNot (Arxiv.Exp $ Arxiv.Abs ["B"]) (Arxiv.Exp $ Arxiv.Abs ["B"]))
                )
             )

testArxiv06 :: Assertion
testArxiv06 = withValidQuery "-A" $ \q ->
  assertBool ("Query not converted into expression: " <> show @(BoolExpr Term) (fromCNF $ getQuery q))
             (Arxiv.qExp (Arxiv.convertQuery q) == Just (
                Arxiv.AndNot (Arxiv.Exp $ Arxiv.Abs ["A"]) (Arxiv.Exp $ Arxiv.Abs ["A"])
                )
             )

--
-- PUBMED tests
--

testPubMed01 :: Assertion
testPubMed01 = withValidQuery "A" $ \q ->
  assertBool ("Query not converted into expression: " <> show @(BoolExpr Term) (fromCNF $ getQuery q))
             (Pubmed.getESearch (Pubmed.convertQuery q) == "A")

testPubMed02_01 :: Assertion
testPubMed02_01 = withValidQuery "-A" $ \q -> Pubmed.getESearch (Pubmed.convertQuery q) @?= "NOT+A"

testPubMed02_02 :: Assertion
testPubMed02_02 = withValidQuery "NOT A" $ \q -> Pubmed.getESearch (Pubmed.convertQuery q) @?= "NOT+A"

testPubMed02_03 :: Assertion
testPubMed02_03 = withValidQuery "NOT (NOT A)" $ \q -> Pubmed.getESearch (Pubmed.convertQuery q) @?= "A"

testPubMed03 :: Assertion
testPubMed03 = withValidQuery "\"Haskell\" AND \"Idris\"" $ \q ->
  Pubmed.getESearch (Pubmed.convertQuery q) @?= "Haskell+AND+Idris"

testPubMed04 :: Assertion
testPubMed04 = withValidQuery "A OR B" $ \q ->
  Pubmed.getESearch (Pubmed.convertQuery q) @?= "A+OR+B"

testPubMedCovid_01 :: IO (Maybe PubmedApiKey) -> Assertion
testPubMedCovid_01 getPubmedKey = do
  mb_key <- getPubmedKey
  case mb_key of
    Nothing -> pure ()
    Just k  -> withValidQuery "\"Covid\"" $ \query -> do
      res <- Pubmed.get (_PubmedApiKey k) (renderQuery query) (Just 1)
      case res of
        Left err       -> fail (show err)
        Right (_, cnd) -> do
          hyperDocs <- sourceToList cnd
          case hyperDocs of
            []    -> fail "No documents found."
            (x:_) -> _hd_title x @?= Just "Being a Hospice Nurse in Times of the COVID-19 Pandemic: A Phenomenological Study of Providing End-of-Life Care."

testPubMedCovid_02 :: IO (Maybe PubmedApiKey) -> Assertion
testPubMedCovid_02 getPubmedKey = do
  mb_key <- getPubmedKey
  case mb_key of
    Nothing -> pure ()
    Just k  -> withValidQuery "\"Covid\" AND \"Alzheimer\"" $ \query -> do
      res <- Pubmed.get (_PubmedApiKey k) (renderQuery query) (Just 1)
      case res of
        Left err       -> fail (show err)
        Right (_, cnd) -> do
          hyperDocs <- sourceToList cnd
          case hyperDocs of
            []    -> fail "No documents found."
            (x:_) -> _hd_title x @?= Just "Neurodegenerative and Neurodevelopmental Diseases and the Gut-Brain Axis: The Potential of Therapeutic Targeting of the Microbiome."

testArxivRealWorld_01 :: IO (Maybe PubmedApiKey) -> Assertion
testArxivRealWorld_01 getPubmedKey = do
  mb_key <- getPubmedKey
  case mb_key of
    Nothing -> pure ()
    Just _  -> withValidQuery "\"Haskell\"" $ \query -> do
      (_, cnd) <- Arxiv.get EN query (Just 1)
      hyperDocs <- sourceToList cnd
      case hyperDocs of
        []    -> fail "No documents found."
        (x:_) -> _hd_title x @?= Just "Haskell for OCaml programmers"

testArxivRealWorld_02 :: IO (Maybe PubmedApiKey) -> Assertion
testArxivRealWorld_02 getPubmedKey = do
  mb_key <- getPubmedKey
  case mb_key of
    Nothing -> pure ()
    Just _  -> withValidQuery "\"Haskell\" AND \"Agda\"" $ \query -> do
      (_, cnd) <- Arxiv.get EN query (Just 1)
      hyperDocs <- sourceToList cnd
      case hyperDocs of
        []    -> fail "No documents found."
        (x:_) -> _hd_title x @?= Just "Toward Hole-Driven Development with Liquid Haskell"
