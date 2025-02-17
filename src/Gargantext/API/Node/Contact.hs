{-|
Module      : Gargantext.API.Node.Contact
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}


module Gargantext.API.Node.Contact
      where

import Conduit ( yield )
import Data.Aeson
import Data.Swagger ( ToSchema )
import Gargantext.API.Admin.Auth.Types ( AuthenticatedUser(AuthenticatedUser) )
import Gargantext.API.Admin.EnvTypes (Env, GargJob(..))
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Errors.Types ( BackendInternalError )
import Gargantext.API.Node ( nodeNodeAPI, NodeNodeAPI )
import Gargantext.API.Prelude (GargM, simuLogs)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Terms (TermType(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Flow (flow)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Types.Hyperdata.Contact ( HyperdataContact, hyperdataContact )
import Gargantext.Database.Admin.Types.Hyperdata.Corpus ( HyperdataAnnuaire(..) )
import Gargantext.Database.Admin.Types.Node ( CorpusId, NodeId )
import Gargantext.Prelude (($), Generic, Maybe(..), Text)
import Gargantext.Utils.Aeson qualified as GUA
import Gargantext.Utils.Jobs (serveJobsAPI, MonadJobStatus(..))
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary ( Arbitrary(arbitrary) )
import Gargantext.Database.Query.Tree.Root (MkCorpusUser(MkCorpusUserNormalCorpusIds))

------------------------------------------------------------------------
type API = "contact" :> Summary "Contact endpoint"
            :> API_Async
          :<|> Capture "contact_id" NodeId
            :> NodeNodeAPI HyperdataContact


api :: AuthenticatedUser -> CorpusId -> ServerT API (GargM Env BackendInternalError)
api authUser@(AuthenticatedUser userNodeId _userUserId) cid =
       (api_async   (RootId userNodeId) cid)
  :<|> (nodeNodeAPI (Proxy :: Proxy HyperdataContact) authUser cid)

type API_Async = AsyncJobs JobLog '[JSON] AddContactParams JobLog
------------------------------------------------------------------------
data AddContactParams = AddContactParams         { firstname :: !Text, lastname :: !Text }
                      | AddContactParamsAdvanced { firstname :: !Text
                                                 , lastname  :: !Text
                                                 -- TODO add others fields
                                                 }
    deriving (Generic)

----------------------------------------------------------------------
api_async :: User -> NodeId -> ServerT API_Async (GargM Env BackendInternalError)
api_async u nId =
  serveJobsAPI AddContactJob $ \jHandle p ->
    addContact u nId p jHandle

addContact :: (HasSettings env, FlowCmdM env err m, MonadJobStatus m)
    => User
    -> NodeId
    -> AddContactParams
    -> JobHandle m
    -> m ()
addContact u nId (AddContactParams fn ln) jobHandle = do

  markStarted 2 jobHandle
  _ <- flow (Nothing :: Maybe HyperdataAnnuaire) (MkCorpusUserNormalCorpusIds u [nId]) (Multi EN) Nothing (1, yield $ hyperdataContact fn ln) jobHandle

  markComplete jobHandle
addContact _uId _nId _p jobHandle = do
  simuLogs jobHandle 10

------------------------------------------------------------------------
-- TODO unPrefix "pn_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  AddContactParams where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = GUA.defaultTaggedObject })

instance ToJSON    AddContactParams where
  toJSON = genericToJSON (defaultOptions { sumEncoding = GUA.defaultTaggedObject })
  
instance ToSchema  AddContactParams
instance Arbitrary AddContactParams where
  arbitrary = elements [AddContactParams "Pierre" "Dupont"]

------------------------------------------------------------------------
