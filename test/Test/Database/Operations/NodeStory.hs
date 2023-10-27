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
import Data.Set qualified as Set
import Database.PostgreSQL.Simple qualified as PSQL
import Database.PostgreSQL.Simple.SqlQQ
import Gargantext.API.Ngrams (setListNgrams, saveNodeStoryImmediate)
import Gargantext.API.Ngrams.Types (MSet(..), NgramsRepoElement(..), NgramsTerm(..), nre_list)
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



createListTest :: TestEnv -> Assertion
createListTest env = do
  flip runReaderT env $ runTestMonad $ do
    -- NOTE(adn) We need to create user 'gargantua'(!!) in order
    -- for 'addDocumentsToHyperCorpus' to work.
    let user = UserName userMaster
    parentId <- getRootId user
    [corpus] <- getCorporaWithParentId parentId
    let corpusId = _node_id corpus

    userId <- getUserId user

    listId <- getOrMkList corpusId userId
    listId' <- getOrMkList corpusId userId
    
    liftIO $ listId `shouldBe` listId'


queryNodeStoryTest :: TestEnv -> Assertion
queryNodeStoryTest env = do
  flip runReaderT env $ runTestMonad $ do
    -- NOTE(adn) We need to create user 'gargantua'(!!) in order
    -- for 'addDocumentsToHyperCorpus' to work.
    let user = UserName userMaster
    parentId <- getRootId user
    [corpus] <- getCorporaWithParentId parentId
    let corpusId = _node_id corpus

    userId <- getUserId user

    listId <- getOrMkList corpusId userId

    saveNodeStoryImmediate

    v <- getNodeStoryVar [listId]
    
    liftIO $ do
      ns <- atomically $ readTVar v
      ns `shouldBe` (NodeStory $ Map.singleton listId
                       (initArchive :: ArchiveList))
  

insertNewTermsToNodeStoryTest :: TestEnv -> Assertion
insertNewTermsToNodeStoryTest env = do
  flip runReaderT env $ runTestMonad $ do
    -- NOTE(adn) We need to create user 'gargantua'(!!) in order
    -- for 'addDocumentsToHyperCorpus' to work.
    let user = UserName userMaster
    parentId <- getRootId user
    [corpus] <- getCorporaWithParentId parentId
    let corpusId = _node_id corpus

    userId <- getUserId user

    listId <- getOrMkList corpusId userId

    v <- getNodeStoryVar [listId]

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
                       ((initArchive :: ArchiveList) & a_state .~
                          Map.singleton NgramsTerms nls))
    -- check that the ngrams are in the DB as well
    ngramsMap <- selectNgramsId [terms]
    -- saveNodeStory is called by `setListNgrams`
    liftIO $ (snd <$> Map.toList ngramsMap) `shouldBe` [terms]
    
    -- _ <- insertNgrams [UnsafeNgrams { _ngramsTerms = terms
    --                                 , _ngramsSize = 1 }]

    -- Finally, check that node stories are inserted correctly
    -- saveNodeStoryImmediate

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
    -- NOTE(adn) We need to create user 'gargantua'(!!) in order
    -- for 'addDocumentsToHyperCorpus' to work.
    let user = UserName userMaster
    parentId <- getRootId user
    [corpus] <- getCorporaWithParentId parentId
    let corpusId = _node_id corpus

    userId <- getUserId user

    listId <- getOrMkList corpusId userId

    v <- getNodeStoryVar [listId]

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
                      ((initArchive :: ArchiveList) & a_state .~
                         Map.singleton NgramsTerms nls))

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
    -- NOTE(adn) We need to create user 'gargantua'(!!) in order
    -- for 'addDocumentsToHyperCorpus' to work.
    let user = UserName userMaster
    parentId <- getRootId user
    [corpus] <- getCorporaWithParentId parentId
    let corpusId = _node_id corpus

    userId <- getUserId user

    listId <- getOrMkList corpusId userId

    v <- getNodeStoryVar [listId]

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
                      ((initArchive :: ArchiveList) & a_state .~
                         Map.singleton NgramsTerms nlsWithChildFixed))

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
    -- NOTE(adn) We need to create user 'gargantua'(!!) in order
    -- for 'addDocumentsToHyperCorpus' to work.
    let user = UserName userMaster
    parentId <- getRootId user
    [corpus] <- getCorporaWithParentId parentId
    let corpusId = _node_id corpus

    userId <- getUserId user

    listId <- getOrMkList corpusId userId

    v <- getNodeStoryVar [listId]

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
                       ((initArchive :: ArchiveList) & a_state .~
                          Map.singleton NgramsTerms nls))
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
                       ((initArchive :: ArchiveList) & a_state .~
                          (Map.singleton NgramsTerms $ nls <> nls2)))


setListNgramsUpdatesNodeStoryWithChildrenTest :: TestEnv -> Assertion
setListNgramsUpdatesNodeStoryWithChildrenTest env = do
  flip runReaderT env $ runTestMonad $ do
    -- NOTE(adn) We need to create user 'gargantua'(!!) in order
    -- for 'addDocumentsToHyperCorpus' to work.
    let user = UserName userMaster
    parentId <- getRootId user
    [corpus] <- getCorporaWithParentId parentId
    let corpusId = _node_id corpus

    userId <- getUserId user

    listId <- getOrMkList corpusId userId

    v <- getNodeStoryVar [listId]

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
                       ((initArchive :: ArchiveList) & a_state .~
                          Map.singleton NgramsTerms nls))

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
                       ((initArchive :: ArchiveList) & a_state .~
                          Map.singleton NgramsTerms nlsNew))
