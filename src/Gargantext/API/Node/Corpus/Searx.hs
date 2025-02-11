{-|
Module      : Gargantext.API.Node.Corpus.Searx
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE TemplateHaskell #-}

module Gargantext.API.Node.Corpus.Searx where

import Control.Lens (view)
import Data.Aeson qualified as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Tuple.Select (sel1, sel2, sel3)
import Gargantext.Core (Lang(..))
import Gargantext.Core.NLP (HasNLPServer, nlpServerGet)
import Gargantext.Core.NodeStory.Types ( HasNodeStory )
import Gargantext.Core.Text.Corpus.API qualified as API
import Gargantext.Core.Text.List (buildNgramsLists)
import Gargantext.Core.Text.List.Group.WithStem ({-StopSize(..),-} GroupParams(..))
import Gargantext.Core.Text.Terms (TermType(..))
import Gargantext.Core.Types (HasValidationError)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Action.Flow (addDocumentsToHyperCorpus) --, DataText(..))
import Gargantext.Database.Action.Flow.List (flowList_DbRepo)
import Gargantext.Database.Action.User (getUserId)
import Gargantext.Database.Admin.Types.Hyperdata.Corpus (HyperdataCorpus)
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Database.Admin.Types.Node (CorpusId, ListId, NodeType(NodeTexts))
import Gargantext.Database.Prelude (hasConfig)
import Gargantext.Database.Query.Table.Node (getOrMkList, insertDefaultNodeIfNotExists)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Tree.Error (HasTreeError)
import Gargantext.Database.Query.Tree.Root (getOrMkRootWithCorpus, MkCorpusUser (MkCorpusUserMaster))
import Gargantext.Prelude hiding (All)
import Gargantext.Prelude.Config
import Gargantext.Utils.Jobs (JobHandle, MonadJobStatus(..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Prelude qualified

langToSearx :: Lang -> Text
langToSearx x   = Text.toLower acronym <> "-" <> acronym
  where
    acronym = show x

data SearxResult = SearxResult
  { _sr_url           :: Text
  , _sr_title         :: Text
  , _sr_content       :: Maybe Text
  , _sr_engine        :: Text
  , _sr_score         :: Double
  , _sr_category      :: Text
  , _sr_pretty_url    :: Text
  , _sr_publishedDate :: Text   -- "Nov 19, 2021"
  , _sr_pubdate       :: Text  -- "2021-11-19 02:12:00+0000"
  }
  deriving (Show, Eq, Generic)
--  , _sr_parsed_url
--  , _sr_engines
--  , _sr_positions

$(deriveJSON (unPrefix "_sr_") ''SearxResult)

data SearxResponse = SearxResponse
  { _srs_query                :: Text
  , _srs_number_of_results    :: Int
  , _srs_results              :: [SearxResult] }
  deriving (Show, Eq, Generic)
-- , _srs_answers
-- , _srs_corrections
-- , _srs_infoboxes
--  , _srs_suggestions          :: [Text]
--  , _srs_unresponsive_engines :: [Text] }

$(deriveJSON (unPrefix "_srs_") ''SearxResponse)

data FetchSearxParams = FetchSearxParams
  { _fsp_language :: Lang
  , _fsp_manager :: Manager
  , _fsp_pageno  :: Int
  , _fsp_query   :: Text
  , _fsp_url     :: Text
  }

fetchSearxPage :: FetchSearxParams -> IO (Either Prelude.String SearxResponse)
fetchSearxPage (FetchSearxParams { _fsp_language
                                 , _fsp_manager
                                 , _fsp_pageno
                                 , _fsp_query
                                 , _fsp_url }) = do
  -- searx search API:
  -- https://searx.github.io/searx/dev/search_api.html?highlight=json
  req <- parseRequest $ T.unpack _fsp_url
  let request = urlEncodedBody
        [ --("category_general", "1")
          ("q", encodeUtf8 _fsp_query)
        , ("categories", "news")  -- https://gitlab.iscpif.fr/gargantext/haskell-gargantext/issues/70#note_3976
        , ("pageno", encodeUtf8 $ T.pack $ show _fsp_pageno)
          --, ("time_range", "None")
        , ("language", encodeUtf8 $ langToSearx _fsp_language)
        , ("format", "json")
        ] req
  res <- httpLbs request _fsp_manager
  let dec = Aeson.eitherDecode $ responseBody res :: (Either Prelude.String SearxResponse)
  pure dec

insertSearxResponse :: ( MonadBase IO m
                       , HasNodeStory env err m
                       , HasNLPServer env
                       , HasNodeError err
                       , HasTreeError err
                       , HasValidationError err )
                    => User
                    -> CorpusId
                    -> ListId
                    -> Lang
                    -> Either Prelude.String SearxResponse
                    -> m ()
insertSearxResponse _ _ _ _ (Left _) = pure ()
insertSearxResponse user cId listId l (Right (SearxResponse { _srs_results })) = do
  server <- view (nlpServerGet l)
  -- docs :: [Either Text HyperdataDocument]
  let docs = hyperdataDocumentFromSearxResult l <$> _srs_results
  --printDebug "[triggerSearxSearch] docs" docs
  let docs' = mapMaybe rightToMaybe docs
    {-
  Prelude.mapM_ (\(HyperdataDocument { _hd_title, _hd_publication_year, _hd_publication_date }) -> do
      printDebug "[triggerSearxSearch] doc time" $
      "[title] " <> (show _hd_title) <>
      " :: [publication_year] " <> (show _hd_publication_year) <>
      " :: [publication_date] " <> (show _hd_publication_date)
    ) docs'
    -}
  --_ <- flowDataText user (DataNew [docs']) (Multi l) cId Nothing logStatus
  let mCorpus = Nothing :: Maybe HyperdataCorpus
  void $ addDocumentsToHyperCorpus server mCorpus (Multi l) cId docs'
  (_masterUserId, _masterRootId, masterCorpusId)
    <- getOrMkRootWithCorpus MkCorpusUserMaster mCorpus
  let gp = GroupWithPosTag l server HashMap.empty
    -- gp = case l of
    --   FR -> GroupWithPosTag l Spacy HashMap.empty
    --   _       -> GroupWithPosTag l CoreNLP HashMap.empty
  ngs         <- buildNgramsLists user cId masterCorpusId Nothing gp
  _userListId <- flowList_DbRepo listId ngs

  pure ()

-- TODO Make an async task out of this?
triggerSearxSearch :: ( MonadBase IO m
                      , HasNodeStory env err m
                      , HasNLPServer env
                      , HasNodeError err
                      , HasTreeError err
                      , HasValidationError err
                      , MonadJobStatus m )
                   => User
                   -> CorpusId
                   -> API.RawQuery
                   -> Lang
                   -> JobHandle m
                   -> m ()
triggerSearxSearch user cId q l jobHandle = do
  userId <- getUserId user

  _tId <- insertDefaultNodeIfNotExists NodeTexts cId userId

  let numPages = 100
  markStarted numPages jobHandle

  -- printDebug "[triggerSearxSearch] cId" cId
  -- printDebug "[triggerSearxSearch] q" q
  -- printDebug "[triggerSearxSearch] l" l
  cfg <- view hasConfig
  uId <- getUserId user
  let surl = _gc_frame_searx_url cfg
  -- printDebug "[triggerSearxSearch] surl" surl
  listId <- getOrMkList cId uId

  -- printDebug "[triggerSearxSearch] listId" listId

  manager <- liftBase $ newManager tlsManagerSettings
  _ <- mapM (\page -> do
                res <- liftBase $ fetchSearxPage $ FetchSearxParams { _fsp_language = l
                                                                    , _fsp_manager = manager
                                                                    , _fsp_pageno = page
                                                                    , _fsp_query = API.getRawQuery q
                                                                    , _fsp_url = surl }

                insertSearxResponse user cId listId l res
                markProgress page jobHandle

            ) [1..numPages]
  --printDebug "[triggerSearxSearch] res" res
  markComplete jobHandle

hyperdataDocumentFromSearxResult :: Lang -> SearxResult -> Either T.Text HyperdataDocument
hyperdataDocumentFromSearxResult l (SearxResult { _sr_content, _sr_engine, _sr_pubdate, _sr_title }) = do
  let mDate = parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S+0000" (T.unpack _sr_pubdate) :: Maybe Day
  let mGregorian = toGregorian <$> mDate
  Right HyperdataDocument { _hd_bdd = Just "Searx"
                          , _hd_doi = Nothing
                          , _hd_url = Nothing
                          , _hd_page = Nothing
                          , _hd_title = Just _sr_title
                          , _hd_authors = Nothing
                          , _hd_institutes = Nothing
                          , _hd_source = Just _sr_engine
                          , _hd_abstract = _sr_content
                          , _hd_publication_date = T.pack Prelude.. formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" Prelude.<$> mDate
                          , _hd_publication_year = fromIntegral Prelude.. sel1 Prelude.<$> mGregorian
                          , _hd_publication_month = sel2 <$> mGregorian
                          , _hd_publication_day = sel3 <$> mGregorian
                          , _hd_publication_hour = Nothing
                          , _hd_publication_minute = Nothing
                          , _hd_publication_second = Nothing
                          , _hd_language_iso2 = Just $ T.pack $ show l }
