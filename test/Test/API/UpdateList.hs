{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}

module Test.API.UpdateList (
    tests
  ) where

import Data.Aeson qualified as JSON
import Data.Aeson.QQ
import Data.ByteString.Lazy qualified as BL
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import Fmt
import Gargantext.API.Admin.Auth.Types (Token)
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Ngrams.List ( ngramsListFromCSVData )
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.User
import Gargantext.Database.Admin.Types.Hyperdata.Corpus
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Tree.Root
import Gargantext.Database.Schema.Ngrams
import Gargantext.Prelude hiding (get)
import Network.Wai.Handler.Warp qualified as Wai
import Paths_gargantext (getDataFileName)
import Prelude (error)
import Test.API.Private (withValidLogin, protectedJSON, postJSONUrlEncoded, getJSON)
import Test.API.Setup (withTestDBAndPort, setupEnvironment, mkUrl, createAliceAndBob)
import Test.Database.Types
import Test.Hspec
import Test.Hspec.Wai.Internal (withApplication, WaiSession)
import Test.Hspec.Wai.JSON (json)
import Test.Hspec.Wai (shouldRespondWith)
import Web.FormUrlEncoded
import qualified Data.Map.Strict as Map

data JobPollHandle = JobPollHandle {
    _jph_id     :: !Text
  , _jph_log    :: [JobLog]
  , _jph_status :: !Text
  , _jph_error  :: !(Maybe Text)
    } deriving Show

instance JSON.FromJSON JobPollHandle where
  parseJSON = JSON.withObject "JobPollHandle" $ \o -> do
    _jph_id     <- o JSON..: "id"
    _jph_log    <- o JSON..: "log"
    _jph_status <- o JSON..: "status"
    _jph_error  <- o JSON..:? "error"
    pure JobPollHandle{..}

instance JSON.ToJSON JobPollHandle where
  toJSON JobPollHandle{..} = JSON.object [
      "id"      JSON..= JSON.toJSON _jph_id
    , "log"     JSON..= JSON.toJSON _jph_log
    , "status"  JSON..= JSON.toJSON _jph_status
    , "error"   JSON..= JSON.toJSON _jph_error
    ]

newCorpusForUser :: TestEnv -> T.Text -> IO NodeId
newCorpusForUser env uname = flip runReaderT env $ runTestMonad $ do
  uid      <- getUserId (UserName uname)
  parentId <- getRootId (UserName uname)
  let corpusName = "Test_Corpus"
  (corpusId:_) <- mk (Just corpusName) (Nothing :: Maybe HyperdataCorpus) parentId uid
  pure corpusId

-- | Poll the given URL every second until it finishes.
-- Retries up to 60 times (i.e. for 1 minute, before giving up)
pollUntilFinished :: HasCallStack
                  => Token
                  -> Wai.Port
                  -> (JobPollHandle -> Builder)
                  -> JobPollHandle
                  -> WaiSession () JobPollHandle
pollUntilFinished tkn port mkUrlPiece = go 60
  where
   go :: Int -> JobPollHandle -> WaiSession () JobPollHandle
   go 0 h  = error $ T.unpack $ "pollUntilFinished exhausted attempts. Last found JobPollHandle: " <> T.decodeUtf8 (BL.toStrict $ JSON.encode h)
   go n h = case _jph_status h == "IsPending" || _jph_status h == "IsRunning" of
    True -> do
      liftIO $ threadDelay 1_000_000
      h' <- protectedJSON tkn "GET" (mkUrl port $ mkUrlPiece h) ""
      go (n-1) h'
    False
     | _jph_status h == "IsFailure"
     -> error $ T.unpack $ "JobPollHandle contains a failure: " <> T.decodeUtf8 (BL.toStrict $ JSON.encode h)
     | otherwise
     -> pure h

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

