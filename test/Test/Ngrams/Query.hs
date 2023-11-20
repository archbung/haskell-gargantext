{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Test.Ngrams.Query (tests) where

import Control.Monad
import Data.Coerce
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Patch.Class qualified as Patch
import Data.Text qualified as T
import Data.Validity qualified as Validity
import Gargantext.API.Ngrams
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types.Main
import Gargantext.Core.Types.Query
import Gargantext.Prelude
import Test.Ngrams.Query.PaginationCorpus
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "Ngrams" [unitTests]

curryElem :: NgramsElement
curryElem = mkNgramsElement "curry" MapTerm Nothing mempty

elbaElem :: NgramsElement
elbaElem = mkNgramsElement "elba" MapTerm Nothing mempty

mockFlatCorpus :: Versioned (Map NgramsTerm NgramsElement)
mockFlatCorpus = Versioned 0 $ Map.fromList [
    ( "haskell", curryElem)
  , ( "idris", elbaElem)
  ]

mockQueryFn :: Maybe T.Text -> NgramsTerm -> Bool
mockQueryFn searchQuery (NgramsTerm nt) =
  maybe (const True) T.isInfixOf (T.toLower <$> searchQuery) (T.toLower nt)

unitTests :: TestTree
unitTests = testGroup "Query tests"
  [ -- Sorting
    testCase "Simple query mockFlatCorpus" testFlat01
  , testCase "Simple query (desc sorting)" testFlat02
  --  -- Filtering
  , testCase "Simple query (listType = MapTerm)" testFlat03
  , testCase "Simple query (listType = StopTerm)" testFlat04
  --  -- Full text search
  , testCase "Simple query (search with match)" testFlat05
  --  -- Pagination
  , testCase "Simple pagination on all terms" test_pagination_allTerms
  , testCase "Simple pagination on MapTerm" test_pagination01
  , testCase "Simple pagination on MapTerm  (limit < total terms)" test_pagination02
  , testCase "Simple pagination on MapTerm  (offset works)" test_pagination02_offset
  , testCase "Simple pagination on ListTerm (limit < total terms)" test_pagination03
  , testCase "Simple pagination on ListTerm  (offset works)" test_pagination03_offset
  , testCase "Simple pagination on CandidateTerm (limit < total terms)" test_pagination04
  , testCase "paginating QuantumComputing corpus works (MapTerms)" test_paginationQuantum
  , testCase "paginating QuantumComputing corpus works (CandidateTerm)" test_paginationQuantum_02
  --  -- Patching
  , testCase "I can apply a patch to term mapTerms to stopTerms (issue #217)" test_217
  ]

-- Let's test that if we request elements sorted in
-- /ascending/ order, we get them.
testFlat01 :: Assertion
testFlat01 = do
  let res = searchTableNgrams mockFlatCorpus searchQuery
  res @?= VersionedWithCount 0 2 ( NgramsTable [curryElem, elbaElem] )
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 5
               , _nsq_offset      = Nothing
               , _nsq_listType    = Nothing
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Just TermAsc
               , _nsq_searchQuery = mockQueryFn Nothing
               }

-- Let's test that if we request elements sorted in
-- /descending/ order, we get them.
testFlat02 :: Assertion
testFlat02 = do
  let res = searchTableNgrams mockFlatCorpus searchQuery
  res @?= VersionedWithCount 0 2 ( NgramsTable [elbaElem, curryElem] )
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 5
               , _nsq_offset      = Nothing
               , _nsq_listType    = Nothing
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Just TermDesc
               , _nsq_searchQuery = mockQueryFn Nothing
               }

testFlat03 :: Assertion
testFlat03 = do
  let res = searchTableNgrams mockFlatCorpus searchQuery
  res @?= VersionedWithCount 0 2 ( NgramsTable [elbaElem, curryElem] )
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 5
               , _nsq_offset      = Nothing
               , _nsq_listType    = Just MapTerm
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Just TermDesc
               , _nsq_searchQuery = mockQueryFn Nothing
               }

-- Here we are searching for all the stop terms, but
-- due to the fact we don't have any inside 'mockFlatCorpus',
-- we should get no results.
testFlat04 :: Assertion
testFlat04 = do
  let res = searchTableNgrams mockFlatCorpus searchQuery
  res @?= VersionedWithCount 0 0 ( NgramsTable [] )
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 5
               , _nsq_offset      = Nothing
               , _nsq_listType    = Just StopTerm
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Just TermDesc
               , _nsq_searchQuery = mockQueryFn Nothing
               }

-- For this test, we run a full text search on the word
-- \"curry\", and we expect back a result.
testFlat05 :: Assertion
testFlat05 = do
  let res = searchTableNgrams mockFlatCorpus searchQuery
  res @?= VersionedWithCount 0 1 ( NgramsTable [curryElem] )
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 5
               , _nsq_offset      = Nothing
               , _nsq_listType    = Nothing
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Just TermDesc
               , _nsq_searchQuery = mockQueryFn (Just "curry")
               }

-- Pagination tests

test_pagination_allTerms :: Assertion
test_pagination_allTerms = do
  let res = searchTableNgrams paginationCorpus searchQuery
  res @?= VersionedWithCount 0 10 ( NgramsTable [ haskellElem
                                                , sideEffectsElem
                                                , concHaskellElem
                                                , implementationElem
                                                , ooElem
                                                , languagesElem
                                                , javaElem
                                                , termsElem
                                                ] )
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 8
               , _nsq_offset      = Nothing
               , _nsq_listType    = Nothing
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Nothing
               , _nsq_searchQuery = mockQueryFn Nothing
               }

-- In this test, I'm asking for 5 /map terms/, and as the
-- corpus has only 2, that's what I should get back.
test_pagination01 :: Assertion
test_pagination01 = do
  let res = searchTableNgrams paginationCorpus searchQuery
  res @?= VersionedWithCount 0 4 ( NgramsTable [implementationElem, languagesElem, termsElem, proofElem] )
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 5
               , _nsq_offset      = Nothing
               , _nsq_listType    = Just MapTerm
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Just ScoreDesc
               , _nsq_searchQuery = mockQueryFn Nothing
               }

test_pagination02 :: Assertion
test_pagination02 = do
  let res = searchTableNgrams paginationCorpus searchQuery
  res @?= VersionedWithCount 0 4 ( NgramsTable [implementationElem, languagesElem, termsElem] )
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 3
               , _nsq_offset      = Nothing
               , _nsq_listType    = Just MapTerm
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Just ScoreDesc
               , _nsq_searchQuery = mockQueryFn Nothing
               }

test_pagination02_offset :: Assertion
test_pagination02_offset = do
  let res = searchTableNgrams paginationCorpus searchQuery
  res @?= VersionedWithCount 0 4 ( NgramsTable [termsElem, proofElem] )
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 2
               , _nsq_offset      = Just (Offset 2)
               , _nsq_listType    = Just MapTerm
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Just ScoreDesc
               , _nsq_searchQuery = mockQueryFn Nothing
               }

test_pagination03 :: Assertion
test_pagination03 = do
  let res = searchTableNgrams paginationCorpus searchQuery
  res @?= VersionedWithCount 0 4 ( NgramsTable [sideEffectsElem, ooElem, javaElem] )
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 3
               , _nsq_offset      = Nothing
               , _nsq_listType    = Just StopTerm
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Just ScoreDesc
               , _nsq_searchQuery = mockQueryFn Nothing
               }

test_pagination03_offset :: Assertion
test_pagination03_offset = do
  let res = searchTableNgrams paginationCorpus searchQuery
  res @?= VersionedWithCount 0 4 ( NgramsTable [javaElem, pascalElem] )
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 2
               , _nsq_offset      = Just (Offset 2)
               , _nsq_listType    = Just StopTerm
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Just ScoreDesc
               , _nsq_searchQuery = mockQueryFn Nothing
               }

test_pagination04 :: Assertion
test_pagination04 = do
  let res = searchTableNgrams paginationCorpus searchQuery
  res @?= VersionedWithCount 0 2 ( NgramsTable [haskellElem] )
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 1
               , _nsq_offset      = Nothing
               , _nsq_listType    = Just CandidateTerm
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Just ScoreDesc
               , _nsq_searchQuery = mockQueryFn Nothing
               }

test_paginationQuantum :: Assertion
test_paginationQuantum = do
  let res = searchTableNgrams quantumComputingCorpus searchQuery
  let elems = coerce @NgramsTable @[NgramsElement] $ _vc_data res
  length elems @?= 10
  forM_ elems $ \term ->
    assertBool ("found " <> show (_ne_list term) <> " in: " <> show elems) (_ne_list term == MapTerm)
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 10
               , _nsq_offset      = Nothing
               , _nsq_listType    = Just MapTerm
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Nothing
               , _nsq_searchQuery = mockQueryFn Nothing
               }

test_paginationQuantum_02 :: Assertion
test_paginationQuantum_02 = do
  let res = searchTableNgrams quantumComputingCorpus searchQuery
  let elems = coerce @NgramsTable @[NgramsElement] $ _vc_data res
  assertBool ("found only " <> show (length elems) <> " in: " <> show elems) (length elems == 10)
  where
   searchQuery = NgramsSearchQuery {
                 _nsq_limit       = Limit 10
               , _nsq_offset      = Nothing
               , _nsq_listType    = Just CandidateTerm
               , _nsq_minSize     = Nothing
               , _nsq_maxSize     = Nothing
               , _nsq_orderBy     = Nothing
               , _nsq_searchQuery = mockQueryFn Nothing
               }

issue217Corpus :: NgramsTableMap
issue217Corpus = Map.fromList [
    ( "advantages", NgramsRepoElement 1 MapTerm Nothing Nothing (mSetFromList ["advantage"]))
  , ( "advantage" , NgramsRepoElement 1 MapTerm (Just "advantages") (Just "advantages") mempty)
  ]

patched217Corpus :: NgramsTableMap
patched217Corpus = Map.fromList [
    ( "advantages", NgramsRepoElement 1 StopTerm Nothing Nothing (mSetFromList ["advantage"]))
  , ( "advantage" , NgramsRepoElement 1 StopTerm (Just "advantages") (Just "advantages") mempty)
  ]

-- In this patch we simulate turning the subtree composed by 'advantages' and 'advantage'
-- from map terms to stop terms.
patch217 :: NgramsTablePatch
patch217 = mkNgramsTablePatch $ Map.fromList [
                                 (NgramsTerm "advantages", NgramsPatch
                                    { _patch_children = mempty
                                    , _patch_list     = Patch.Replace MapTerm StopTerm
                                    }
                                 )
                              ]

test_217 :: Assertion
test_217 = do
  -- Check the patch is applicable
  Validity.validationIsValid (Patch.applicable patch217 (Just issue217Corpus)) @?= True
  Patch.act patch217 (Just issue217Corpus) @?= Just patched217Corpus
