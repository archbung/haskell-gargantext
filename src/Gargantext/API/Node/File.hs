{-|
Module      : Gargantext.API.Node.File
Description :
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-matches #-}

{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE TypeOperators     #-}

{-# LANGUAGE IncoherentInstances #-}

module Gargantext.API.Node.File where

import Control.Lens ((^.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.MIME.Types qualified as DMT
import Data.Swagger
import Data.Text qualified as T
import Gargantext.API.Admin.EnvTypes (GargJob(..), Env)
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Node.Types
import Gargantext.API.Prelude
import Gargantext.Core.Types (TODO)
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Action.Node (mkNodeWithParent)
import Gargantext.Database.Admin.Types.Hyperdata.File
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.GargDB qualified as GargDB
import Gargantext.Database.Query.Table.Node (getNodeWith)
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude
import Gargantext.Utils.Jobs (serveJobsAPI, MonadJobStatus(..))
import Network.HTTP.Media qualified as M
import Servant

data RESPONSE deriving Typeable

instance Accept RESPONSE where
  contentType _ = "text" M.// "*"

instance MimeRender RESPONSE BSResponse where
  mimeRender _ (BSResponse val) = BSL.fromStrict $ val

type FileApi = Summary "File download"
            :> "download"
            :> Get '[RESPONSE] (Headers '[Servant.Header "Content-Type" Text] BSResponse)

instance MimeUnrender RESPONSE BSResponse where
  mimeUnrender _ lbs = Right $ BSResponse (BSL.toStrict lbs)

fileApi :: UserId -> NodeId -> GargServer FileApi
fileApi uId nId = fileDownload uId nId

newtype Contents = Contents BS.ByteString

instance GargDB.ReadFile Contents where
  readFile' fp = do
    c <- BS.readFile fp
    pure $ Contents c

newtype BSResponse = BSResponse BS.ByteString
  deriving (Generic)

instance ToSchema BSResponse  where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy TODO)

fileDownload :: (HasSettings env, FlowCmdM env err m)
             => UserId
             -> NodeId
             -> m (Headers '[Servant.Header "Content-Type" Text] BSResponse)
fileDownload uId nId = do
  -- printDebug "[fileDownload] uId" uId
  -- printDebug "[fileDownload] nId" nId

  node <- getNodeWith nId (Proxy :: Proxy HyperdataFile)
  let (HyperdataFile { _hff_name = name'
                     , _hff_path = path }) = node ^. node_hyperdata

  Contents c <- GargDB.readGargFile $ T.unpack path

  let (mMime, _) = DMT.guessType DMT.defaultmtd False $ T.unpack name'
      mime = case mMime of
        Just m  -> m
        Nothing -> "text/plain"

  pure $ addHeader (T.pack mime) $ BSResponse c
 
  --pure c

  -- let settings = embeddedSettings [("", encodeUtf8 c)]

  -- Tagged $ staticApp settings

  -- let settings = embeddedSettings [("", "hello")]
  -- Tagged $ staticApp settings

type FileAsyncApi = Summary "File Async Api"
                 :> "file"
                 :> "add"
                 :> AsyncJobs JobLog '[FormUrlEncoded] NewWithFile JobLog

fileAsyncApi :: UserId -> NodeId -> ServerT FileAsyncApi (GargM Env GargError)
fileAsyncApi uId nId =
  serveJobsAPI AddFileJob $ \jHandle i ->
    addWithFile uId nId i jHandle


addWithFile :: (HasSettings env, FlowCmdM env err m, MonadJobStatus m)
            => UserId
            -> NodeId
            -> NewWithFile
            -> JobHandle m
            -> m ()
addWithFile uId nId nwf@(NewWithFile _d _l fName) jobHandle = do

  -- printDebug "[addWithFile] Uploading file: " nId
  markStarted 1 jobHandle

  fPath <- GargDB.writeFile nwf
  -- printDebug "[addWithFile] File saved as: " fPath

  nIds <- mkNodeWithParent NodeFile (Just nId) uId fName

  _ <- case nIds of
    [nId'] -> do
        node <- getNodeWith nId' (Proxy :: Proxy HyperdataFile)
        let hl = node ^. node_hyperdata
        _ <- updateHyperdata nId' $ hl { _hff_name = fName
                                       , _hff_path = T.pack fPath }

        -- printDebug "[addWithFile] Created node with id: " nId'
        pure ()
    _     -> pure ()

  -- printDebug "[addWithFile] File upload finished: " nId
  markComplete jobHandle
