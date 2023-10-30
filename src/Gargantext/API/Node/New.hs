{-|
Module      : Gargantext.API.Node.Post
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

New = Post maybe change the name
Async new node feature

-}

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE IncoherentInstances #-}
module Gargantext.API.Node.New
      where

import Control.Lens hiding (elements, Empty)
import Data.Aeson
import Data.Swagger
import Gargantext.API.Admin.Auth.Types
import Gargantext.API.Admin.EnvTypes (GargJob(..), Env)
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Errors.Types
import Gargantext.API.Prelude
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Action.Node
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Prelude
import Gargantext.Utils.Jobs (serveJobsAPI, MonadJobStatus(..))
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import Web.FormUrlEncoded (FromForm, ToForm)

------------------------------------------------------------------------
data PostNode = PostNode { pn_name     :: Text
                         , pn_typename :: NodeType}
  deriving (Generic)
------------------------------------------------------------------------
-- TODO unPrefix "pn_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  PostNode
instance ToJSON    PostNode
instance ToSchema  PostNode
instance FromForm  PostNode
instance ToForm    PostNode
instance Arbitrary PostNode where
  arbitrary = elements [PostNode "Node test" NodeCorpus]

------------------------------------------------------------------------
postNode :: HasNodeError err
         => AuthenticatedUser
         -- ^ The logged-in user
         -> NodeId
         -> PostNode
         -> Cmd err [NodeId]
postNode authenticatedUser pId (PostNode nodeName nt) = do
  let userId = authenticatedUser ^. auth_user_id
  mkNodeWithParent nt (Just pId) userId nodeName

------------------------------------------------------------------------
type PostNodeAsync = Summary "Post Node"
   :> "async"
   :> AsyncJobs JobLog '[FormUrlEncoded] PostNode JobLog


postNodeAsyncAPI
  :: AuthenticatedUser
  -- ^ The logged-in user
  -> NodeId
  -- ^ The target node
  -> ServerT PostNodeAsync (GargM Env BackendInternalError)
postNodeAsyncAPI authenticatedUser nId =
  serveJobsAPI NewNodeJob $ \jHandle p -> postNodeAsync authenticatedUser nId p jHandle

------------------------------------------------------------------------
postNodeAsync :: (FlowCmdM env err m, MonadJobStatus m)
    => AuthenticatedUser
    -- ^ The logged in user
    -> NodeId
    -> PostNode
    -> JobHandle m
    -> m ()
postNodeAsync authenticatedUser nId (PostNode nodeName tn) jobHandle = do

  -- printDebug "postNodeAsync" nId
  markStarted 3 jobHandle
  markProgress 1 jobHandle

  -- _ <- threadDelay 1000
  markProgress 1 jobHandle

  let userId = authenticatedUser ^. auth_user_id
  _ <- mkNodeWithParent tn (Just nId) userId nodeName

  markComplete jobHandle
