{-|
Module      : Gargantext.API.Node.Corpus.New
Description : New corpus API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

New corpus means either:
- new corpus
- new data in existing corpus
-}

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}

module Gargantext.API.Node.Corpus.New
      where


import Conduit
import Control.Lens hiding (elements, Empty)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Base64 qualified as BSB64
import Data.Conduit.Internal (zipSources)
import Data.Swagger
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import EPO.API.Client.Types qualified as EPO
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Ngrams (commitStatePatch, Versioned(..))
import Gargantext.API.Node.Corpus.New.Types
import Gargantext.API.Node.Corpus.Searx
import Gargantext.API.Node.Corpus.Types
import Gargantext.API.Node.Corpus.Update (addLanguageToCorpus)
import Gargantext.API.Node.Types
import Gargantext.Core (Lang(..), withDefaultLanguage, defaultLanguage)
import Gargantext.Core.NodeStory (HasNodeStoryImmediateSaver, HasNodeArchiveStoryImmediateSaver, currentVersion, NgramsStatePatch')
import Gargantext.Core.Text.Corpus.API qualified as API
import Gargantext.Core.Text.Corpus.Parsers qualified as Parser (FileType(..), parseFormatC)
import Gargantext.Core.Text.List.Social (FlowSocialListWith(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Action.Flow (flowCorpus, getDataText, flowDataText, TermType(..){-, allDataOrigins-})
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Action.Mail (sendMail)
import Gargantext.Database.Action.Node (mkNodeWithParent)
import Gargantext.Database.Action.User (getUserId)
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node (CorpusId, NodeType(..), ParentId)
import Gargantext.Database.GargDB qualified as GargDB
import Gargantext.Database.Prelude (hasConfig)
import Gargantext.Database.Query.Table.Node (getNodeWith, getOrMkList)
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude
import Gargantext.Prelude.Config (gc_max_docs_parsers)
import Gargantext.System.Logging
import Gargantext.Utils.Jobs (JobHandle, MonadJobStatus(..))
import Servant
import Servant.Job.Utils (jsonOptions)
import Test.QuickCheck.Arbitrary

------------------------------------------------------------------------
{-
data Query = Query { query_query      :: Text
                   , query_node_id    :: Int
                   , query_lang       :: Lang
                   , query_databases  :: [DataOrigin]
                   }
                   deriving (Eq, Generic)

deriveJSON (unPrefix "query_") 'Query

instance Arbitrary Query where
    arbitrary = elements [ Query q n la fs
                         | q <- ["honeybee* AND collapse"
                                ,"covid 19"
                                ]
                         , n <- [0..10]
                         , la <- allLangs
                         , fs <- take 3 $ repeat allDataOrigins
                         ]

instance ToSchema Query where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "query_")
-}

------------------------------------------------------------------------

{-
type Api = PostApi
        :<|> GetApi

type PostApi = Summary "New Corpus endpoint"
             :> ReqBody '[JSON] Query
             :> Post    '[JSON] CorpusId
type GetApi = Get '[JSON] ApiInfo
-}

-- | TODO manage several apis
-- TODO-ACCESS
-- TODO this is only the POST
{-
api :: (FlowCmdM env err m) => UserId -> Query -> m CorpusId
api uid (Query q _ as) = do
  cId <- case head as of
    Nothing      -> flowCorpusSearchInDatabase (UserDBId uid) EN q
    Just API.All -> flowCorpusSearchInDatabase (UserDBId uid) EN q
    Just a   -> do
      docs <- liftBase $ API.get a q (Just 1000)
      cId' <- flowCorpus (UserDBId uid) (Left q) (Multi EN) [docs]
      pure cId'

  pure cId
-}

------------------------------------------------
-- TODO use this route for Client implementation
data ApiInfo = ApiInfo { api_info :: [API.ExternalAPIs]}
  deriving (Generic)
instance Arbitrary ApiInfo where
  arbitrary = ApiInfo <$> arbitrary

deriveJSON (unPrefix "") 'ApiInfo

instance ToSchema ApiInfo

info :: ApiInfo
info = ApiInfo API.externalAPIs

------------------------------------------------------------------------
------------------------------------------------------------------------
data WithQuery = WithQuery
  { _wq_query        :: !API.RawQuery
  , _wq_databases    :: !Database
  , _wq_datafield    :: !(Maybe Datafield)
  , _wq_lang         :: !Lang
  , _wq_node_id      :: !Int
  , _wq_flowListWith :: !FlowSocialListWith
  , _wq_pubmedAPIKey :: !(Maybe Text)
  , _wq_epoAPIUser   :: !(Maybe Text)
  , _wq_epoAPIToken  :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

makeLenses ''WithQuery
instance FromJSON WithQuery where
  parseJSON = genericParseJSON $ jsonOptions "_wq_"
instance ToJSON WithQuery where
  toJSON = genericToJSON $ jsonOptions "_wq_"
instance ToSchema WithQuery where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wq_")

instance Arbitrary WithQuery where
  arbitrary = WithQuery <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary

------------------------------------------------------------------------

type AddWithQuery = Summary "Add with Query to corpus endpoint"
   :> "corpus"
     :> Capture "corpus_id" CorpusId
   :> "query"
     :> AsyncJobs JobLog '[JSON] WithQuery JobLog

{-
type AddWithFile = Summary "Add with MultipartData to corpus endpoint"
   :> "corpus"
     :> Capture "corpus_id" CorpusId
   :> "add"
   :> "file"
     :> MultipartForm Mem (MultipartData Mem)
     :> QueryParam "fileType"  FileType
   :> "async"
     :> AsyncJobs JobLog '[JSON] () JobLog
-}


------------------------------------------------------------------------
-- TODO WithQuery also has a corpus id


addToCorpusWithQuery :: ( FlowCmdM env err m
                        , MonadJobStatus m
                        , HasNodeStoryImmediateSaver env
                        , HasNodeArchiveStoryImmediateSaver env )
                       => User
                       -> CorpusId
                       -> WithQuery
                       -> Maybe API.Limit
                       -> JobHandle m
                       -> m ()
addToCorpusWithQuery user cid (WithQuery { _wq_query = q
                                         , _wq_databases = dbs
                                         , _wq_datafield = datafield
                                         , _wq_lang = l
                                         , _wq_flowListWith = flw
                                         , _wq_pubmedAPIKey = mPubmedAPIKey
                                         , .. }) maybeLimit jobHandle = do
  -- TODO ...
  $(logLocM) DEBUG $ T.pack $ "(cid, dbs) " <> show (cid, dbs)
  $(logLocM) DEBUG $ T.pack $ "datafield  " <> show datafield
  $(logLocM) DEBUG $ T.pack $ "flowListWith " <> show flw

  let mEPOAuthKey = EPO.AuthKey <$> (EPO.User <$> _wq_epoAPIUser)
                                <*> (EPO.Token <$> _wq_epoAPIToken)

  addLanguageToCorpus cid l

  case datafield of
    Just Web -> do
      $(logLocM) DEBUG $ T.pack $ "processing web request " <> show datafield

      markStarted 1 jobHandle

      _ <- triggerSearxSearch user cid q l jobHandle

      markComplete jobHandle

    _ -> do
      markStarted 3 jobHandle

      -- TODO add cid
      -- TODO if cid is folder -> create Corpus
      --      if cid is corpus -> add to corpus
      --      if cid is root   -> create corpus in Private
      $(logLocM) DEBUG $ T.pack $ "getDataText with query: " <> show q
      let db = database2origin dbs
      -- mPubmedAPIKey <- getUserPubmedAPIKey user
      -- printDebug "[addToCorpusWithQuery] mPubmedAPIKey" mPubmedAPIKey
      eTxt <- getDataText db (Multi l) q mPubmedAPIKey mEPOAuthKey maybeLimit

      -- printDebug "[G.A.N.C.New] lTxts" lTxts
      case eTxt of
        Right txt -> do
          -- TODO Sum lenghts of each txt elements
          $(logLocM) DEBUG "Processing dataText results"
          markProgress 1 jobHandle

          corpusId <- flowDataText user txt (Multi l) cid (Just flw) jobHandle
          $(logLocM) DEBUG $ T.pack $ "corpus id " <> show corpusId

          _ <- commitCorpus cid user
          
          -- printDebug "sending email" ("xxxxxxxxxxxxxxxxxxxxx" :: Text)
          sendMail user
          -- TODO ...
          markComplete jobHandle

        Left err -> do
          -- printDebug "Error: " err
          $(logLocM) ERROR (T.pack $ show err)
          markFailed (Just $ T.pack (show err)) jobHandle

type AddWithForm = Summary "Add with FormUrlEncoded to corpus endpoint"
   :> "corpus"
     :> Capture "corpus_id" CorpusId
   :> "add"
   :> "form"
   :> "async"
     :> AsyncJobs JobLog '[FormUrlEncoded] NewWithForm JobLog

addToCorpusWithForm :: ( FlowCmdM env err m
                       , MonadJobStatus m
                       , HasNodeStoryImmediateSaver env
                       , HasNodeArchiveStoryImmediateSaver env )
                    => User
                    -> CorpusId
                    -> NewWithForm
                    -> JobHandle m
                    -> m ()
addToCorpusWithForm user cid nwf jobHandle = do
  -- printDebug "[addToCorpusWithForm] Parsing corpus: " cid
  -- printDebug "[addToCorpusWithForm] fileType" ft
  -- printDebug "[addToCorpusWithForm] fileFormat" ff

  let l = nwf ^. wf_lang . non defaultLanguage
  addLanguageToCorpus cid l

  limit' <- view $ hasConfig . gc_max_docs_parsers
  let limit = fromIntegral limit' :: Integer
  let
    parseC = case (nwf ^. wf_filetype) of
      CSV       -> Parser.parseFormatC Parser.CsvGargV3
      CSV_HAL   -> Parser.parseFormatC Parser.CsvHal
      Iramuteq  -> Parser.parseFormatC Parser.Iramuteq
      Istex     -> Parser.parseFormatC Parser.Istex
      JSON      -> Parser.parseFormatC Parser.JSON
      PresseRIS -> Parser.parseFormatC Parser.RisPresse
      WOS       -> Parser.parseFormatC Parser.WOS

  -- TODO granularity of the logStatus
  let data' = case (nwf ^. wf_fileformat) of
        Plain -> cs (nwf ^. wf_data)
        ZIP   -> case BSB64.decode $ TE.encodeUtf8 (nwf ^. wf_data) of
          Left err -> panicTrace $ T.pack "[addToCorpusWithForm] error decoding base64: " <> T.pack err
          Right decoded -> decoded
  eDocsC <- liftBase $ parseC (nwf ^. wf_fileformat) data'
  case eDocsC of
    Right (count, docsC) -> do
      -- TODO Add progress (jobStatus) update for docs - this is a
      -- long action

      let docsC' = zipSources (yieldMany [1..]) docsC
                  .| mapMC (\(idx, doc) ->
                        if idx > limit then do
                          --printDebug "[addToCorpusWithForm] number of docs exceeds the limit" (show limit)
                          let panicMsg' = [ "[addToCorpusWithForm] number of docs "
                                          , "exceeds the MAX_DOCS_PARSERS limit ("
                                          , show limit
                                          , ")" ]
                          let panicMsg = T.concat $ T.pack <$> panicMsg'
                          --logStatus $ jobLogFailTotalWithMessage panicMsg jobLog
                          panicTrace panicMsg
                        else
                          pure doc)
                  .| mapC toHyperdataDocument

      --printDebug "Parsing corpus finished : " cid
      --logStatus jobLog2

      --printDebug "Starting extraction     : " cid
      -- TODO granularity of the logStatus
      -- printDebug "flowCorpus with (corpus_id, lang)" (cid, l)

      _cid' <- flowCorpus user
                          (Right [cid])
                          (Multi l)
                          (Just (nwf ^. wf_selection))
                          --(Just $ fromIntegral $ length docs, docsC')
                          (count, transPipe liftBase docsC') -- TODO fix number of docs
                          --(map (map toHyperdataDocument) docs)
                          jobHandle

      _ <- commitCorpus cid user

      -- printDebug "Extraction finished   : " cid
      -- printDebug "sending email" ("xxxxxxxxxxxxxxxxxxxxx" :: Text)
      -- TODO uncomment this
      --sendMail user

      markComplete jobHandle
    Left e -> do
      printDebug "[addToCorpusWithForm] parse error" e
      markFailed (Just e) jobHandle

{-
addToCorpusWithFile :: FlowCmdM env err m
                    => CorpusId
                    -> MultipartData Mem
                    -> Maybe FileType
                    -> (JobLog -> m ())
                    -> m JobLog
addToCorpusWithFile cid input filetype logStatus = do
  logStatus JobLog { _scst_succeeded = Just 10
                          , _scst_failed    = Just 2
                          , _scst_remaining = Just 138
                          , _scst_events    = Just []
                          }
  printDebug "addToCorpusWithFile" cid
  _h <- postUpload cid filetype input

  pure      JobLog { _scst_succeeded = Just 137
                          , _scst_failed    = Just 13
                          , _scst_remaining = Just 0
                          , _scst_events    = Just []
                          }
-}



type AddWithFile = Summary "Add with FileUrlEncoded to corpus endpoint"
   :> "corpus"
     :> Capture "corpus_id" CorpusId
   :> "add"
   :> "file"
   :> "async"
     :> AsyncJobs JobLog '[FormUrlEncoded] NewWithFile JobLog

addToCorpusWithFile :: (HasSettings env, FlowCmdM env err m, MonadJobStatus m)
                    => User
                    -> CorpusId
                    -> NewWithFile
                    -> JobHandle m
                    -> m ()
addToCorpusWithFile user cid nwf@(NewWithFile _d (withDefaultLanguage -> l) fName) jobHandle = do

  addLanguageToCorpus cid l

  printDebug "[addToCorpusWithFile] Uploading file to corpus: " cid
  markStarted 1 jobHandle

  fPath <- GargDB.writeFile nwf
  printDebug "[addToCorpusWithFile] File saved as: " fPath

  uId <- getUserId user
  nIds <- mkNodeWithParent NodeFile (Just cid) uId fName

  _ <- case nIds of
    [nId] -> do
        node <- getNodeWith nId (Proxy :: Proxy HyperdataFile)
        let hl = node ^. node_hyperdata
        _ <- updateHyperdata nId $ hl { _hff_name = fName
                                      , _hff_path = T.pack fPath }

        printDebug "[addToCorpusWithFile] Created node with id: " nId
    _     -> pure ()

  printDebug "[addToCorpusWithFile] File upload to corpus finished: " cid

  printDebug "sending email" ("xxxxxxxxxxxxxxxxxxxxx" :: Text)
  sendMail user

  markComplete jobHandle



--- UTILITIES

commitCorpus :: ( FlowCmdM env err m
                , HasNodeArchiveStoryImmediateSaver env
                , HasNodeStoryImmediateSaver env )
             => ParentId -> User -> m (Versioned NgramsStatePatch')
commitCorpus cid user = do  
  userId <- getUserId user
  listId <- getOrMkList cid userId
  v <- currentVersion listId
  commitStatePatch listId (Versioned v mempty)
