{-|
Module      : Gargantext.API.Node.DocumentsFromWriteNodes
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Gargantext.API.Node.DocumentsFromWriteNodes
      where

import Conduit
import Control.Lens ((^.))
import Data.Aeson
import Data.List qualified as List
import Data.Swagger
import Data.Text qualified as T
import Gargantext.API.Admin.Auth.Types
import Gargantext.API.Admin.EnvTypes (Env, GargJob(..))
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Errors.Types
import Gargantext.API.Ngrams (commitStatePatch, Versioned(..))
import Gargantext.API.Prelude (GargM)
import Gargantext.Core (Lang(..))
import Gargantext.Core.NodeStory (HasNodeStoryImmediateSaver, HasNodeArchiveStoryImmediateSaver, currentVersion)
import Gargantext.Core.Text.Corpus.Parsers.Date (split')
import Gargantext.Core.Text.Corpus.Parsers.FrameWrite
import Gargantext.Core.Text.List.Social (FlowSocialListWith)
import Gargantext.Core.Text.Terms (TermType(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Flow (flowDataText, DataText(..))
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Database.Admin.Types.Hyperdata.Frame
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node (getChildrenByType, getClosestParentIdByType', getNodeWith, getOrMkList)
import Gargantext.Database.Schema.Node (node_hyperdata, node_name, node_date)
import Gargantext.Prelude
import Gargantext.Utils.Jobs (serveJobsAPI, MonadJobStatus(..))
import Servant

------------------------------------------------------------------------
type API = Summary " Documents from Write nodes."
         :> AsyncJobs JobLog '[JSON] Params JobLog
------------------------------------------------------------------------
data Params = Params
  { id         :: Int
  , paragraphs :: Text
  , lang       :: Lang
  , selection  :: FlowSocialListWith
  }
  deriving (Generic, Show)
instance FromJSON Params where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON Params where
  toJSON = genericToJSON defaultOptions
instance ToSchema Params
------------------------------------------------------------------------
api :: AuthenticatedUser
    -- ^ The logged-in user
    -> NodeId
    -> ServerT API (GargM Env BackendInternalError)
api authenticatedUser nId =
  serveJobsAPI DocumentFromWriteNodeJob $ \jHandle p ->
    documentsFromWriteNodes authenticatedUser nId p jHandle

documentsFromWriteNodes :: ( HasSettings env
                           , FlowCmdM env err m
                           , MonadJobStatus m
                           , HasNodeStoryImmediateSaver env
                           , HasNodeArchiveStoryImmediateSaver env )
                        => AuthenticatedUser
                        -- ^ The logged-in user
                        -> NodeId
                        -> Params
                        -> JobHandle m
                        -> m ()
documentsFromWriteNodes authenticatedUser nId Params { selection, lang, paragraphs } jobHandle = do
  markStarted 2 jobHandle
  markProgress 1 jobHandle

  mcId <- getClosestParentIdByType' nId NodeCorpus
  cId <- case mcId of
    Just cId -> pure cId
    Nothing -> do
      let msg = T.pack $ "[G.A.N.DFWN] Node has no corpus parent: " <> show nId
      markFailed (Just msg) jobHandle
      panicTrace msg

  frameWriteIds <- getChildrenByType nId Notes

  -- https://write.frame.gargantext.org/<frame_id>/download
  frameWrites <- mapM (\id -> getNodeWith id (Proxy :: Proxy HyperdataFrame)) frameWriteIds

  frameWritesWithContents <- liftBase $
    mapM (\node -> do
             contents <- getHyperdataFrameContents (node ^. node_hyperdata)
             pure (node, contents)
         ) frameWrites

  let paragraphs' = fromMaybe (7 :: Int) $ (readMaybe $ T.unpack paragraphs)
  let parsedE = (\(node, contents)
                  -> hyperdataDocumentFromFrameWrite lang paragraphs' (node, contents)) <$> frameWritesWithContents
  let parsed = List.concat $ rights parsedE
  -- printDebug "DocumentsFromWriteNodes: uId" uId
  _ <- flowDataText (RootId userNodeId)
                    (DataNew (Just $ fromIntegral $ length parsed, yieldMany parsed))
                    (Multi lang)
                    cId
                    (Just selection)
                    jobHandle


  -- FIXME(adn) If we were to store the UserID inside an 'AuthenticatedUser', we won't need this.
  listId <- getOrMkList cId userId
  v <- currentVersion listId
  _ <- commitStatePatch listId (Versioned v mempty)

  markProgress 1 jobHandle
  where
    userNodeId = authenticatedUser ^. auth_node_id
    userId     = authenticatedUser ^. auth_user_id

------------------------------------------------------------------------
hyperdataDocumentFromFrameWrite :: Lang -> Int -> (Node HyperdataFrame, T.Text) -> Either T.Text [HyperdataDocument]
hyperdataDocumentFromFrameWrite lang paragraphSize (node, contents) =
  case parseLines contents of
    Left _ -> Left "Error parsing node"
    Right (Parsed { authors, contents = ctxts}) ->
      let HyperdataFrame { _hf_base, _hf_frame_id } = node ^. node_hyperdata
          authorJoinSingle (Author { firstName, lastName }) = T.concat [ lastName, ", ", firstName ]
          authors' = T.concat $ authorJoinSingle <$> authors

--{-
          (year',month',day') = split' (node^. node_date)
          date' = Just $ T.concat [ T.pack $ show year', "-"
                                  , T.pack $ show month', "-"
                                  , T.pack $ show day'
                                  ]
--}

{-
          date' = (\(Date { year, month, day }) -> T.concat [ T.pack $ show year', "-"
                                                            , T.pack $ show month', "-"
                                                            , T.pack $ show day' ]) <$> date
          year' = fromIntegral $ maybe Defaults.year (\(Date { year }) -> year) date
          month' = maybe Defaults.month (\(Date { month }) -> fromIntegral month) date
          day' = maybe Defaults.day (\(Date { day }) -> fromIntegral day) date
--}
          in
      Right (List.map (\(t, ctxt) ->  HyperdataDocument { _hd_bdd = Just $ show Notes
                              , _hd_doi = Nothing
                              , _hd_url = Nothing
                              , _hd_uniqId = Nothing
                              , _hd_uniqIdBdd = Nothing
                              , _hd_page = Nothing
                              , _hd_title = Just t
                              , _hd_authors = Just authors'
                              , _hd_institutes = Nothing
                              , _hd_source = Just $ node ^. node_name
                              , _hd_abstract = Just ctxt
                              , _hd_publication_date = date'
                              , _hd_publication_year = Just year'
                              , _hd_publication_month = Just month'
                              , _hd_publication_day = Just day'
                              , _hd_publication_hour = Nothing
                              , _hd_publication_minute = Nothing
                              , _hd_publication_second = Nothing
                              , _hd_language_iso2 = Just $ T.pack $ show lang }
                      ) (text2titleParagraphs paragraphSize ctxts)
                  )
