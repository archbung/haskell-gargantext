{-|
Module      : Gargantext.API.Node.FrameCalcUpload
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE TypeOperators       #-}

module Gargantext.API.Node.FrameCalcUpload where

import Control.Lens ((^.))
import Data.Aeson ( FromJSON, ToJSON )
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.UTF8 qualified as BSU8
import Data.Swagger ( ToSchema )
import Data.Text qualified as T
import Gargantext.API.Admin.Auth.Types ( auth_node_id, AuthenticatedUser )
import Gargantext.API.Admin.EnvTypes (GargJob(..), Env)
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Errors.Types ( BackendInternalError )
import Gargantext.API.Node.Corpus.New (addToCorpusWithForm)
import Gargantext.API.Node.Corpus.New.Types (FileFormat(..), FileType(..))
import Gargantext.API.Node.Types (NewWithForm(..))
import Gargantext.API.Prelude ( GargM )
import Gargantext.Core (Lang)
import Gargantext.Core.NodeStory.Types ( HasNodeArchiveStoryImmediateSaver )
import Gargantext.Core.Text.List.Social (FlowSocialListWith(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Flow.Types ( FlowCmdM )
import Gargantext.Database.Admin.Types.Hyperdata.Frame ( HyperdataFrame(..) )
import Gargantext.Database.Admin.Types.Node ( NodeId, NodeType(NodeCorpus) )
import Gargantext.Database.Prelude (HasConfig)
import Gargantext.Database.Query.Table.Node (getClosestParentIdByType, getNodeWith)
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude
import Gargantext.Utils.Jobs (serveJobsAPI, MonadJobStatus(..))
import Network.HTTP.Client (newManager, httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant ( type (:>), JSON, Summary, HasServer(ServerT) )
import Web.FormUrlEncoded (FromForm)


data FrameCalcUpload = FrameCalcUpload {
  _wf_lang      :: !(Maybe Lang)
, _wf_selection :: !FlowSocialListWith
}
  deriving (Generic)

instance FromForm FrameCalcUpload
instance FromJSON FrameCalcUpload
instance ToJSON FrameCalcUpload
instance ToSchema FrameCalcUpload

type API = Summary " FrameCalc upload"
           :> "add"
           :> "framecalc"
           :> "async"
           :> AsyncJobs JobLog '[JSON] FrameCalcUpload JobLog

api :: AuthenticatedUser -> NodeId -> ServerT API (GargM Env BackendInternalError)
api authenticatedUser nId =
  serveJobsAPI UploadFrameCalcJob $ \jHandle p ->
    frameCalcUploadAsync authenticatedUser nId p jHandle



frameCalcUploadAsync :: ( HasConfig env
                        , FlowCmdM env err m
                        , MonadJobStatus m
                        , HasNodeArchiveStoryImmediateSaver env )
                     => AuthenticatedUser
                     -- ^ The logged-in user
                     -> NodeId
                     -> FrameCalcUpload
                     -> JobHandle m
                     -> m ()
frameCalcUploadAsync authenticatedUser nId (FrameCalcUpload _wf_lang _wf_selection) jobHandle = do
  markStarted 5 jobHandle

  -- printDebug "[frameCalcUploadAsync] uId" uId
  -- printDebug "[frameCalcUploadAsync] nId" nId

  node <- getNodeWith nId (Proxy :: Proxy HyperdataFrame)
  let (HyperdataFrame { _hf_base = base
                      , _hf_frame_id = frame_id }) = node ^. node_hyperdata

  let csvUrl = base <> "/" <> frame_id <> ".csv"
  -- printDebug "[frameCalcUploadAsync] csvUrl" csvUrl

  res <- liftBase $ do
    manager <- newManager tlsManagerSettings
    req <- parseRequest $ T.unpack csvUrl
    httpLbs req manager
  let body = T.pack $ BSU8.toString $ BSL.toStrict $ responseBody res

  -- printDebug "body" body
  mCId <- getClosestParentIdByType nId NodeCorpus
  -- printDebug "[frameCalcUploadAsync] mCId" mCId

  case mCId of
    Nothing -> markFailure 1 Nothing jobHandle
    Just cId ->
      -- FIXME(adn) Audit this conversion.
      addToCorpusWithForm (RootId userNodeId)
                          cId
                          (NewWithForm { _wf_filetype = CSV
                                       , _wf_fileformat = Plain
                                       , _wf_data = body
                                       , _wf_lang
                                       , _wf_name = "calc-upload.csv"
                                       , _wf_selection }) jobHandle

  markComplete jobHandle
  where
    userNodeId = authenticatedUser ^. auth_node_id
