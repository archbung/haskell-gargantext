{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Test.API.UpdateList (
    tests
  , newCorpusForUser
  , JobPollHandle(..)
  , pollUntilFinished
  ) where

import Control.Lens ((^.), mapped, over, view)
import Control.Monad.Fail (fail)
import Data.Aeson.QQ
import Data.Map.Strict qualified as Map
import Data.Map.Strict.Patch qualified as PM
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Fmt
import Gargantext.API.Admin.Auth.Types (Token)
import Gargantext.API.Ngrams qualified as APINgrams
import Gargantext.API.Ngrams.List ( ngramsListFromCSVData )
import Gargantext.API.Ngrams.Types ( MSet(..), NgramsPatch(..), NgramsRepoElement(..), NgramsTablePatch(..), NgramsTerm(..), Versioned(..), mSetToList, toNgramsPatch, ne_children, ne_ngrams, vc_data, _NgramsTable )
import Gargantext.Core.NodeStory (hasNodeStory, nse_getter, HasNodeArchiveStoryImmediateSaver(..))
import Gargantext.Core.Types ( CorpusId, ListId, ListType(..), NodeId, _NodeId )
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.User
import Gargantext.Database.Admin.Types.Hyperdata.Corpus
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Tree.Root
import Gargantext.Database.Schema.Ngrams
import Gargantext.Prelude hiding (get)
import Network.Wai.Handler.Warp qualified as Wai
import Paths_gargantext (getDataFileName)
import Servant.Client (runClientM)
import Test.API.Routes (mkUrl, table_ngrams_get_api, table_ngrams_put_api)
import Test.API.Setup (withTestDBAndPort, setupEnvironment, createAliceAndBob)
import Test.Database.Types
import Test.Hspec
import Test.Hspec.Wai (shouldRespondWith)
import Test.Hspec.Wai.Internal (withApplication, WaiSession)
import Test.Hspec.Wai.JSON (json)
import Test.Types (JobPollHandle(..))
import Test.Utils (authenticatedServantClient, getJSON, pollUntilFinished, postJSONUrlEncoded, protectedJSON, withValidLogin)
import Web.FormUrlEncoded


newCorpusForUser :: TestEnv -> T.Text -> IO NodeId
newCorpusForUser env uname = flip runReaderT env $ runTestMonad $ do
  uid      <- getUserId (UserName uname)
  parentId <- getRootId (UserName uname)
  let corpusName = "Test_Corpus"
  (corpusId:_) <- mk (Just corpusName) (Nothing :: Maybe HyperdataCorpus) parentId uid
  pure corpusId

uploadJSONList :: Wai.Port -> Token -> CorpusId -> WaiSession () ListId
uploadJSONList port token cId = do
  ([listId] :: [NodeId]) <- protectedJSON token "POST" (mkUrl port ("/node/" <> build cId)) [aesonQQ|{"pn_typename":"NodeList","pn_name":"Testing"}|]
  -- Upload the JSON doc
  simpleNgrams <- liftIO (TIO.readFile =<< getDataFileName "test-data/ngrams/simple.json")
  let jsonFileFormData = [ (T.pack "_wjf_data", simpleNgrams)
                         , ("_wjf_filetype", "JSON")
                         , ("_wjf_name", "simple_ngrams.json")
                         ]
  let url         = "/lists/"  +|listId|+ "/add/form/async"
  let mkPollUrl j = "/corpus/" +|listId|+ "/add/form/async/" +|_jph_id j|+ "/poll?limit=1"
  (j :: JobPollHandle) <- postJSONUrlEncoded token (mkUrl port url) (urlEncodeFormStable $ toForm jsonFileFormData)
  j' <- pollUntilFinished token port mkPollUrl j
  liftIO (_jph_status j' `shouldBe` "IsFinished")

  pure listId

-- uploadListPatch :: Wai.Port
--                 -> Token
--                 -> CorpusId
--                 -> ListId
--                 -> APINgrams.Version
--                 -> PM.PatchMap NgramsTerm NgramsPatch
--                 -> WaiSession () ()
-- uploadListPatch port token cId listId version patch = do
--   let js = JSON.toJSON (Versioned version $ NgramsTablePatch patch)
--   -- panicTrace $ "[uploadListPatch] js: " <> show js
--   -- APINgrams.tableNgramsPut Terms listId (Versioned 0 $ NgramsTablePatch $ fst patch)
--   (_res :: Versioned NgramsTablePatch) <- protectedJSON token "PUT" (mkUrl port ("/node/" <> build cId <> "/ngrams?ngramsType=Terms&list=" <> build listId)) js
--   -- panicTrace $ "[uploadListPatch] res: " <> show res
--   pure ()


tests :: Spec
tests = sequential $ aroundAll withTestDBAndPort $ do
  describe "UpdateList API" $ do
    it "setup DB triggers and users" $ \((testEnv, _), _) -> do
      setupEnvironment testEnv
      createAliceAndBob testEnv

    describe "POST /api/v1.0/lists/:id/add/form/async (JSON)" $ do

      it "allows uploading a JSON ngrams file" $ \((testEnv, port), app) -> do
        cId <- newCorpusForUser testEnv "alice"
        withApplication app $ do
          withValidLogin port "alice" (GargPassword "alice") $ \token -> do
            listId <- uploadJSONList port token cId

            -- Now check that we can retrieve the ngrams
            let getUrl = "/node/" +| listId |+ "/ngrams?ngramsType=Terms&listType=MapTerm&list="+| listId |+"&limit=50"
            getJSON token (mkUrl port getUrl)
              `shouldRespondWith` [json| { "version": 0,
                                            "count": 1,
                                            "data": [
                                                {
                                                    "ngrams": "abelian group",
                                                    "size": 2,
                                                    "list": "MapTerm",
                                                    "occurrences": [],
                                                    "children": []
                                                }
                                            ]
                                        } |]

      it "does not create duplicates when uploading JSON (#313)" $ \((testEnv, port), app) -> do
        cId <- newCorpusForUser testEnv "alice"
        withApplication app $ do
          withValidLogin port "alice" (GargPassword "alice") $ \token -> do
            -- this term is imported from the .json file
            let importedTerm = NgramsTerm "abelian group"
            -- this is the new term, under which importedTerm will be grouped
            let newTerm = NgramsTerm "new abelian group"

            clientEnv <- liftIO $ authenticatedServantClient port token

            listId <- uploadJSONList port token cId
            
            let checkNgrams expected = do
                  eng <- liftIO $ runClientM (table_ngrams_get_api cId APINgrams.Terms listId 10 Nothing (Just MapTerm) Nothing Nothing Nothing Nothing) clientEnv
                  case eng of
                    Left err -> fail (show err)
                    Right r ->
                      let real = over mapped (\nt -> ( nt ^. ne_ngrams
                                                     , mSetToList $ nt ^. ne_children ))
                                             (r ^. vc_data . _NgramsTable) in
                      liftIO $ Set.fromList real `shouldBe` Set.fromList expected
            
            -- The #313 error is about importedTerm being duplicated
            -- in a specific case

            checkNgrams [ (importedTerm, []) ]
            let nre = NgramsRepoElement 1 MapTerm Nothing Nothing (MSet mempty)
            let patch = PM.fromList [
                           ( newTerm
                           , NgramsReplace { _patch_old = Nothing
                                           , _patch_new = Just nre } )
                      ]
            _ <- liftIO $ runClientM (table_ngrams_put_api cId APINgrams.Terms listId (Versioned 1 $ NgramsTablePatch $ fst patch)) clientEnv

            -- check that new term is added (with no parent)
            checkNgrams [ (newTerm, [])
                        , (importedTerm, []) ]

            -- now patch it so that we have a group
            let patchChildren = PM.fromList [
                                 ( newTerm
                                 , toNgramsPatch [importedTerm] )
                               ]
            _ <- liftIO $ runClientM (table_ngrams_put_api cId APINgrams.Terms listId (Versioned 32 $ NgramsTablePatch $ fst patchChildren)) clientEnv

            -- check that new term is parent of old one
            checkNgrams [ (newTerm, [importedTerm]) ]

            -- finally, upload the list again, the group should be as
            -- it was before (the bug in #313 was that "abelian group"
            -- was created again as a term with no parent)
            _ <- uploadJSONList port token cId

            -- old (imported) term shouldn't become parentless
            -- (#313 error was that we had [newTerm, importedTerm] instead)

            -- NOTE: Unfortunately, I'm not able to reproduce this
            -- error here, though I tried. Something is missing, maybe
            -- some nodestory integration with tests?
            checkNgrams [ (newTerm, [importedTerm]) ]

            pure ()

    describe "POST /api/v1.0/lists/:id/csv/add/form/async (CSV)" $ do

      it "parses CSV via ngramsListFromCSVData" $ \((_testEnv, _port), _app) -> do
        simpleNgrams <- liftIO (TIO.readFile =<< getDataFileName "test-data/ngrams/simple.csv")
        ngramsListFromCSVData simpleNgrams `shouldBe`
          Right (Map.fromList [ (NgramsTerms, Versioned 0 $ Map.fromList [
                                  (NgramsTerm "abelian group", NgramsRepoElement 1 MapTerm Nothing Nothing (MSet mempty))
                                , (NgramsTerm "brazorf", NgramsRepoElement 1 StopTerm Nothing Nothing (MSet mempty))
                              ])])

      it "allows uploading a CSV ngrams file" $ \((testEnv, port), app) -> do
        cId <- newCorpusForUser testEnv "alice"
        withApplication app $ do
          withValidLogin port "alice" (GargPassword "alice") $ \token -> do
            ([listId] :: [NodeId]) <- protectedJSON token "POST" (mkUrl port ("/node/" <> build cId)) [aesonQQ|{"pn_typename":"NodeList","pn_name":"Testing"}|]
            -- Upload the CSV doc
            simpleNgrams <- liftIO (TIO.readFile =<< getDataFileName "test-data/ngrams/simple.csv")
            let tsvFileFormData = [ (T.pack "_wtf_data", simpleNgrams)
                                  , ("_wtf_filetype", "CSV")
                                  , ("_wtf_name", "simple.csv")
                                  ]
            let url         = "/lists/"  <> fromString (show $ _NodeId listId) <> "/csv/add/form/async"
            let mkPollUrl j = "/corpus/" <> fromString (show $ _NodeId listId) <> "/add/form/async/" +|_jph_id j|+ "/poll?limit=1"
            (j :: JobPollHandle) <- postJSONUrlEncoded token (mkUrl port url) (urlEncodeFormStable $ toForm tsvFileFormData)
            j' <- pollUntilFinished token port mkPollUrl j
            liftIO (_jph_status j' `shouldBe` "IsFinished")

            -- Now check that we can retrieve the ngrams
            let getTermsUrl = "/node/" +| listId |+ "/ngrams?ngramsType=Terms&listType=MapTerm&list="+| listId |+"&limit=50"
            getJSON token (mkUrl port getTermsUrl)
              `shouldRespondWith` [json| {"version":0
                                         ,"count":1
                                         ,"data":[
                                           {"ngrams":"abelian group"
                                           ,"size":1
                                           ,"list":"MapTerm"
                                           ,"occurrences":[],"children":[]}
                                           ]
                                         } |]
            let getStopUrl = "/node/" +| listId |+ "/ngrams?ngramsType=Terms&listType=StopTerm&list="+| listId |+"&limit=50"
            getJSON token (mkUrl port getStopUrl)
              `shouldRespondWith` [json| {"version":0
                                         ,"count":1
                                         ,"data":[
                                           {"ngrams":"brazorf"
                                           ,"size":1
                                           ,"list":"StopTerm"
                                           ,"occurrences":[],"children":[]}
                                           ]
                                         } |]

