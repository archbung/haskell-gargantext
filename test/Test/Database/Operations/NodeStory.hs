{-|
Module      : Test.Database.Operations.NodeStory
Description : GarganText database tests
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Database.Operations.NodeStory where

import Control.Lens ((.~))
import Control.Monad.Reader
import Data.Map.Strict qualified as Map
import Data.Map.Strict.Patch qualified as PM
import Data.Set qualified as Set
import Database.PostgreSQL.Simple qualified as PSQL
import Database.PostgreSQL.Simple.SqlQQ
import Gargantext.API.Ngrams (commitStatePatch, setListNgrams, saveNodeStoryImmediate)
import Gargantext.API.Ngrams.Types (MSet(..), NgramsPatch(..), NgramsRepoElement(..), NgramsTerm(..), Versioned(..), mkNgramsTablePatch, nre_list)
import Gargantext.API.Ngrams.Tools (getNodeStoryVar)
import Gargantext.Core.NodeStory hiding (runPGSQuery)
import Gargantext.Core.Types.Individu
import Gargantext.Core.Types (ListType(..))
import Gargantext.Database.Action.User (getUserId)
import Gargantext.Database.Admin.Config (userMaster)
import Gargantext.Database.Prelude (runPGSQuery)
import Gargantext.Database.Query.Table.Ngrams (selectNgramsId)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Tree.Root
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Database.Schema.Node (NodePoly(..))
import Gargantext.Prelude
import GHC.Conc (readTVar)
import Test.Database.Types
import Test.Hspec.Expectations
import Test.Tasty.HUnit

commonInitialization :: TestMonad
                        ( Gargantext.Database.Admin.Types.Node.UserId
                        , Gargantext.Database.Admin.Types.Node.NodeId
                        , Gargantext.Database.Admin.Types.Node.ListId
                        , GHC.Conc.Sync.TVar NodeListStory )
commonInitialization = do
  let user = UserName userMaster
  parentId <- getRootId user
  [corpus] <- getCorporaWithParentId parentId
  let corpusId = _node_id corpus

  userId <- getUserId user

  listId <- getOrMkList corpusId userId

  v <- getNodeStoryVar [listId]

  pure $ (userId, corpusId, listId, v)


createListTest :: TestEnv -> Assertion
createListTest env = do
  flip runReaderT env $ runTestMonad $ do
    (userId, corpusId, listId, _v) <- commonInitialization

    listId' <- getOrMkList corpusId userId
    
    liftIO $ listId `shouldBe` listId'


queryNodeStoryTest :: TestEnv -> Assertion
queryNodeStoryTest env = do
  flip runReaderT env $ runTestMonad $ do
    (_userId, _corpusId, listId, v) <- commonInitialization

    saveNodeStoryImmediate
    
    liftIO $ do
      ns <- atomically $ readTVar v
      ns `shouldBe` (NodeStory $ Map.singleton listId
                       (initArchive :: ArchiveList))
  

insertNewTermsToNodeStoryTest :: TestEnv -> Assertion
insertNewTermsToNodeStoryTest env = do
  flip runReaderT env $ runTestMonad $ do
    (_userId, _corpusId, listId, v) <- commonInitialization

    let nre = NgramsRepoElement { _nre_size = 1
                                , _nre_list = MapTerm
                                , _nre_root = Nothing
                                , _nre_parent = Nothing
                                , _nre_children =  MSet Map.empty }
    let terms = "hello"
    let nls = Map.singleton (NgramsTerm terms) nre
    setListNgrams listId NgramsTerms nls
    
    liftIO $ do
      ns <- atomically $ readTVar v
      ns `shouldBe` (NodeStory $ Map.singleton listId
                       ((initArchive :: ArchiveList) { _a_state = Map.singleton NgramsTerms nls }))
    -- check that the ngrams are in the DB as well
    ngramsMap <- selectNgramsId [terms]
    liftIO $ (snd <$> Map.toList ngramsMap) `shouldBe` [terms]
    
    -- Finally, check that node stories are inserted correctly
    dbTerms <- runPGSQuery [sql|
                               SELECT terms
                               FROM ngrams
                               JOIN node_stories ON ngrams.id = ngrams_id
                               WHERE node_id = ?
                               |] (PSQL.Only listId)
    liftIO $ dbTerms `shouldBe` [PSQL.Only terms]
                           

insertNewTermsWithChildrenToNodeStoryTest :: TestEnv -> Assertion
insertNewTermsWithChildrenToNodeStoryTest env = do
  flip runReaderT env $ runTestMonad $ do
    (_userId, _corpusId, listId, v) <- commonInitialization

    let tParent = NgramsTerm "hello"
    let tChild = NgramsTerm "world"
    let terms = unNgramsTerm <$> [tParent, tChild]
    let nreParent = NgramsRepoElement { _nre_size = 1
                                      , _nre_list = MapTerm
                                      , _nre_root = Nothing
                                      , _nre_parent = Nothing
                                      , _nre_children = MSet $ Map.singleton tChild () }
    let nreChild = NgramsRepoElement { _nre_size = 1
                                     , _nre_list = MapTerm
                                     , _nre_root = Just tParent
                                     , _nre_parent = Just tParent
                                     , _nre_children = MSet Map.empty }
    
    let nls = Map.fromList [(tParent, nreParent), (tChild, nreChild)]
    setListNgrams listId NgramsTerms nls

    liftIO $ do
      ns <- atomically $ readTVar v
      ns `shouldBe` (NodeStory $ Map.singleton listId 
                      ((initArchive :: ArchiveList) { _a_state = Map.singleton NgramsTerms nls }))

    -- `setListNgrams` calls saveNodeStory already so we should have
    -- the terms in the DB by now
    ngramsMap <- selectNgramsId terms
    liftIO $ (snd <$> Map.toList ngramsMap) `shouldBe` terms
    
    dbTerms <- runPGSQuery [sql|
                               SELECT terms
                               FROM ngrams
                               JOIN node_stories ON ngrams.id = ngrams_id
                               WHERE node_id = ?
                               |] (PSQL.Only listId)
    liftIO $ (Set.fromList $ (\(PSQL.Only t) -> t) <$> dbTerms) `shouldBe` (Set.fromList terms)

    -- let (Just (tParentId, _)) = head $ filter ((==) (unNgramsTerm tParent) . snd) $ Map.toList ngramsMap2
    -- let (Just (tChildId, _)) = head $ filter ((==) (unNgramsTerm tChild) . snd) $ Map.toList ngramsMap2
                           
    -- [PSQL.Only tParentId'] <-
    --   runPGSQuery [sql|SELECT parent_id FROM ngrams WHERE terms = ?|] (PSQL.Only tChild)

    -- liftIO $ tParentId `shouldBe` tParentId'

      
insertNodeStoryChildrenWithDifferentNgramsTypeThanParentTest :: TestEnv -> Assertion
insertNodeStoryChildrenWithDifferentNgramsTypeThanParentTest env = do
  flip runReaderT env $ runTestMonad $ do
    (_userId, _corpusId, listId, v) <- commonInitialization

    let tParent = NgramsTerm "hello"
    let tChild = NgramsTerm "world"
    let terms = unNgramsTerm <$> [tParent, tChild]
    let nreParent = NgramsRepoElement { _nre_size = 1
                                      , _nre_list = MapTerm
                                      , _nre_root = Nothing
                                      , _nre_parent = Nothing
                                      , _nre_children = MSet $ Map.singleton tChild () }
    let nreChild = NgramsRepoElement { _nre_size = 1
                                     , _nre_list = CandidateTerm
                                     , _nre_root = Just tParent
                                     , _nre_parent = Just tParent
                                     , _nre_children = MSet Map.empty }
    let nreChildFixedType = nreChild & nre_list .~ MapTerm
    
    let nls = Map.fromList [(tParent, nreParent), (tChild, nreChild)]
    let nlsWithChildFixed = Map.fromList [(tParent, nreParent), (tChild, nreChildFixedType)]
    
    setListNgrams listId NgramsTerms nls

    liftIO $ do
      ns <- atomically $ readTVar v
      ns `shouldBe` (NodeStory $ Map.singleton listId 
                      ((initArchive :: ArchiveList) { _a_state = Map.singleton NgramsTerms nlsWithChildFixed }))

    ngramsMap <- selectNgramsId terms
    liftIO $ (snd <$> Map.toList ngramsMap) `shouldBe` terms
    
    dbTerms <- runPGSQuery [sql|
                               SELECT terms
                               FROM ngrams
                               JOIN node_stories ON ngrams.id = ngrams_id
                               WHERE node_id = ?
                               |] (PSQL.Only listId)
    liftIO $ (Set.fromList $ (\(PSQL.Only t) -> t) <$> dbTerms) `shouldBe` (Set.fromList terms)

    let (Just (tChildId, _)) = head $ filter ((==) (unNgramsTerm tChild) . snd) $ Map.toList ngramsMap
                           
    [PSQL.Only childType] <- runPGSQuery [sql|SELECT ngrams_repo_element->>'list'
                                             FROM node_stories
                                             WHERE ngrams_id = ?|] (PSQL.Only tChildId)
    liftIO $ childType `shouldBe` ("MapTerm" :: Text)
 
setListNgramsUpdatesNodeStoryTest :: TestEnv -> Assertion
setListNgramsUpdatesNodeStoryTest env = do
  flip runReaderT env $ runTestMonad $ do
    (_userId, _corpusId, listId, v) <- commonInitialization

    let nre = NgramsRepoElement { _nre_size = 1
                                , _nre_list = MapTerm
                                , _nre_root = Nothing
                                , _nre_parent = Nothing
                                , _nre_children =  MSet Map.empty }
    let terms = "HELLO"
    let nls = Map.singleton (NgramsTerm terms) nre
    setListNgrams listId NgramsTerms nls
    
    liftIO $ do
      ns <- atomically $ readTVar v
      ns `shouldBe` (NodeStory $ Map.singleton listId
                       ((initArchive :: ArchiveList) { _a_state = Map.singleton NgramsTerms nls }))
    -- check that the ngrams are in the DB as well
    ngramsMap <- selectNgramsId [terms]
    liftIO $ (snd <$> Map.toList ngramsMap) `shouldBe` [terms]
    
    let nre2 = NgramsRepoElement { _nre_size = 1
                                 , _nre_list = MapTerm
                                 , _nre_root = Nothing
                                 , _nre_parent = Nothing
                                 , _nre_children =  MSet Map.empty }
    let terms2 = "WORLD"
    let nls2 = Map.singleton (NgramsTerm terms2) nre2
    setListNgrams listId NgramsTerms nls2
                                        
    liftIO $ do
      ns <- atomically $ readTVar v
      ns `shouldBe` (NodeStory $ Map.singleton listId
                       ((initArchive :: ArchiveList) { _a_state = Map.singleton NgramsTerms $ nls <> nls2 }))


setListNgramsUpdatesNodeStoryWithChildrenTest :: TestEnv -> Assertion
setListNgramsUpdatesNodeStoryWithChildrenTest env = do
  flip runReaderT env $ runTestMonad $ do
    (_userId, _corpusId, listId, v) <- commonInitialization

    let tParent = NgramsTerm "hello"
    let tChild = NgramsTerm "world"
    let nreParent = NgramsRepoElement { _nre_size = 1
                                      , _nre_list = MapTerm
                                      , _nre_root = Nothing
                                      , _nre_parent = Nothing
                                      , _nre_children = MSet $ Map.singleton tChild () }
    let nreChild = NgramsRepoElement { _nre_size = 1
                                     , _nre_list = MapTerm
                                     , _nre_root = Just tParent
                                     , _nre_parent = Just tParent
                                     , _nre_children = MSet Map.empty }
    let nls = Map.fromList [(tParent, nreParent), (tChild, nreChild)]
    setListNgrams listId NgramsTerms nls
    
    liftIO $ do
      ns <- atomically $ readTVar v
      ns `shouldBe` (NodeStory $ Map.singleton listId
                       ((initArchive :: ArchiveList) { _a_state = Map.singleton NgramsTerms nls }))

    -- OK, now we substitute parent with no children, the parent of
    -- 'nreChild' should become Nothing
    let nreParentNew = nreParent { _nre_children = MSet $ Map.empty }
    let nlsToInsert = Map.fromList [(tParent, nreParentNew)]
    setListNgrams listId NgramsTerms nlsToInsert

    let nreChildNew = nreChild { _nre_parent = Nothing
                               , _nre_root = Nothing }
    let nlsNew = Map.fromList [(tParent, nreParentNew), (tChild, nreChildNew)]

    liftIO $ do
      ns <- atomically $ readTVar v
      ns `shouldBe` (NodeStory $ Map.singleton listId
                       ((initArchive :: ArchiveList) { _a_state = Map.singleton NgramsTerms nlsNew }))


commitPatchSimpleTest :: TestEnv -> Assertion
commitPatchSimpleTest env = do
  flip runReaderT env $ runTestMonad $ do
    (_userId, _corpusId, listId, v) <- commonInitialization

    -- initially, the node story table is empty
    liftIO $ do
      ns <- atomically $ readTVar v
      ns `shouldBe` (NodeStory $ Map.singleton listId
                       ((initArchive :: ArchiveList) { _a_state = Map.empty }))
    
    let term = NgramsTerm "hello"
    let nre = NgramsRepoElement { _nre_size = 1
                                , _nre_list = MapTerm
                                , _nre_root = Nothing
                                , _nre_parent = Nothing
                                , _nre_children = MSet Map.empty }
    let tPatch = NgramsReplace { _patch_old = Nothing
                               , _patch_new = Just nre }
    ver <- currentVersion listId
    let ntp = mkNgramsTablePatch $ Map.singleton term tPatch
    let (pm, _validation) = PM.singleton NgramsTerms ntp
    let patch = Versioned ver pm

    _patchApplied <- commitStatePatch listId patch

    let nls = Map.fromList [(term, nre)]
    
    liftIO $ do
      ns <- atomically $ readTVar v
      ns `shouldBe` (NodeStory $ Map.singleton listId
                       ((initArchive :: ArchiveList) { _a_state = Map.singleton NgramsTerms nls
                                                     , _a_version = ver + 1 }))
