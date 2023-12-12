{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}

module Test.API.Phylo (
    tests
  ) where

import Data.Aeson.QQ
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Fmt
import Gargantext.API.Ngrams.List ( ngramsListFromCSVData )
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types
import Gargantext.Core.Types.Individu
import Gargantext.Database.Schema.Ngrams
import Gargantext.Prelude hiding (get)
import Paths_gargantext (getDataFileName)
import Test.API.Private (withValidLogin, protectedJSON, postJSONUrlEncoded, getJSON)
import Test.API.Setup (withTestDBAndPort, setupEnvironment, mkUrl, createAliceAndBob)
import Test.API.UpdateList (JobPollHandle(..), pollUntilFinished, newCorpusForUser)
import Test.Hspec
import Test.Hspec.Wai.Internal (withApplication)
import Test.Hspec.Wai.JSON (json)
import Test.Hspec.Wai (shouldRespondWith)
import Web.FormUrlEncoded
import qualified Data.Map.Strict as Map

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
            let url         = "/lists/"  <> (fromString $ show $ _NodeId listId) <> "/csv/add/form/async"
            let mkPollUrl j = "/corpus/" <> (fromString $ show $ _NodeId listId) <> "/add/form/async/" +|_jph_id j|+ "/poll?limit=1"
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

