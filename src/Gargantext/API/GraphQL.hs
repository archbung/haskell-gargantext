{-|
Module      : Gargantext.API.GraphQL
Description :
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fprint-potential-instances #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}  -- permit duplications for field names in multiple constructors
{-# LANGUAGE KindSignatures #-}  -- for use of Endpoint (name :: Symbol)
{-# LANGUAGE PartialTypeSignatures #-}  -- to automatically use suggested type hole signatures during compilation
{-# LANGUAGE TypeOperators #-}

module Gargantext.API.GraphQL where

import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Morpheus ( App, deriveApp )
import Data.Morpheus.Server ( httpPlayground )
import Data.Morpheus.Subscriptions ( Event (..), httpPubApp )
import Data.Morpheus.Types ( GQLRequest, GQLResponse, GQLType, RootResolver(..), Undefined(..) )
import Data.Proxy
import Gargantext.API.Admin.Auth.Types (AuthenticatedUser)
import Gargantext.API.Admin.Orchestrator.Types (JobLog)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Auth.PolicyCheck
import Gargantext.API.Errors.Types
import Gargantext.API.GraphQL.Annuaire qualified as GQLA
import Gargantext.API.GraphQL.AsyncTask qualified as GQLAT
import Gargantext.API.GraphQL.Context qualified as GQLCTX
import Gargantext.API.GraphQL.IMT qualified as GQLIMT
import Gargantext.API.GraphQL.NLP qualified as GQLNLP
import Gargantext.API.GraphQL.Node qualified as GQLNode
import Gargantext.API.GraphQL.Team qualified as GQLTeam
import Gargantext.API.GraphQL.TreeFirstLevel qualified as GQLTree
import Gargantext.API.GraphQL.User qualified as GQLUser
import Gargantext.API.GraphQL.UserInfo qualified as GQLUserInfo
import Gargantext.API.Prelude (GargM)
import Gargantext.API.Prelude (HasJobEnv')
import Gargantext.API.Types
import Gargantext.Core.NLP (HasNLPServer)
import Gargantext.Database.Prelude (CmdCommon)
import Gargantext.Prelude hiding (ByteString)
import Servant
  ( (:<|>) (..)
  , (:>)
  , Get
  , JSON
  , Post
  , ReqBody
  ,  ServerT
  )
import Servant.Auth qualified as SA
import Servant.Auth.Server qualified as SAS


-- | Represents possible GraphQL queries.
data Query m
  = Query
    { annuaire_contacts   :: GQLA.AnnuaireContactArgs -> m [GQLA.AnnuaireContact]
    , context_ngrams      :: GQLCTX.ContextNgramsArgs -> m [Text]
    , contexts            :: GQLCTX.NodeContextArgs -> m [GQLCTX.NodeContextGQL]
    , contexts_for_ngrams :: GQLCTX.ContextsForNgramsArgs -> m [GQLCTX.ContextGQL]
    , imt_schools         :: GQLIMT.SchoolsArgs -> m [GQLIMT.School]
    , job_logs            :: GQLAT.JobLogArgs -> m (Map Int JobLog)
    , languages           :: GQLNLP.LanguagesArgs -> m GQLNLP.LanguagesMap
    , nodes               :: GQLNode.NodeArgs -> m [GQLNode.Node]
    , nodes_corpus        :: GQLNode.CorpusArgs -> m [GQLNode.Corpus]
    , node_parent         :: GQLNode.NodeParentArgs -> m [GQLNode.Node]
    , user_infos          :: GQLUserInfo.UserInfoArgs -> m [GQLUserInfo.UserInfo]
    , users               :: GQLUser.UserArgs -> m [GQLUser.User m]
    , tree                :: GQLTree.TreeArgs -> m (GQLTree.TreeFirstLevel m)
    , team                :: GQLTeam.TeamArgs -> m GQLTeam.Team
    , tree_branch         :: GQLTree.BreadcrumbArgs -> m (GQLTree.BreadcrumbInfo)
    } deriving (Generic, GQLType)

data Mutation m
  = Mutation
    { update_user_info       :: GQLUserInfo.UserInfoMArgs -> m Int
    , update_user_pubmed_api_key :: GQLUser.UserPubmedAPIKeyMArgs -> m Int
    , update_user_epo_api_user :: GQLUser.UserEPOAPIUserMArgs -> m Int
    , update_user_epo_api_token :: GQLUser.UserEPOAPITokenMArgs -> m Int
    , delete_team_membership :: GQLTeam.TeamDeleteMArgs -> m [Int]
    , update_node_context_category :: GQLCTX.NodeContextCategoryMArgs -> m [Int]
    } deriving (Generic, GQLType)

-- | Possible GraphQL Events, i.e. here we describe how we will
-- manipulate the data.
type EVENT m = Event Channel (Contet m)

-- | Channels are possible actions to call when manipulating the data.
data Channel
  = Update
  | New
  deriving (Eq, Show, Generic, Hashable)

-- | This type describes what data we will operate on.
data Contet m
  = UserContet [GQLUser.User m]
  | UserInfoContet [GQLUserInfo.UserInfo]

-- | The main GraphQL resolver: how queries, mutations and
-- subscriptions are handled.
rootResolver
  :: (CmdCommon env, HasNLPServer env, HasJobEnv' env, HasSettings env)
  => AuthenticatedUser
  -> AccessPolicyManager
  -> RootResolver (GargM env BackendInternalError) e Query Mutation Undefined
rootResolver authenticatedUser policyManager =
  RootResolver
    { queryResolver = Query { annuaire_contacts   = GQLA.resolveAnnuaireContacts
                            , context_ngrams      = GQLCTX.resolveContextNgrams
                            , contexts            = GQLCTX.resolveNodeContext
                            , contexts_for_ngrams = GQLCTX.resolveContextsForNgrams
                            , imt_schools         = GQLIMT.resolveSchools
                            , job_logs            = GQLAT.resolveJobLogs
                            , languages           = GQLNLP.resolveLanguages
                            , nodes               = GQLNode.resolveNodes authenticatedUser policyManager
                            , nodes_corpus        = GQLNode.resolveNodesCorpus
                            , node_parent         = GQLNode.resolveNodeParent
                            , user_infos          = GQLUserInfo.resolveUserInfos authenticatedUser policyManager
                            , users               = GQLUser.resolveUsers authenticatedUser policyManager
                            , tree                = GQLTree.resolveTree authenticatedUser policyManager
                            , team                = GQLTeam.resolveTeam 
                            , tree_branch         = GQLTree.resolveBreadcrumb }
    , mutationResolver = Mutation { update_user_info       = GQLUserInfo.updateUserInfo
                                  , update_user_pubmed_api_key = GQLUser.updateUserPubmedAPIKey
                                  , update_user_epo_api_user = GQLUser.updateUserEPOAPIUser
                                  , update_user_epo_api_token = GQLUser.updateUserEPOAPIToken
                                  , delete_team_membership = GQLTeam.deleteTeamMembership
                                  , update_node_context_category = GQLCTX.updateNodeContextCategory }
    , subscriptionResolver = Undefined }

-- | Main GraphQL "app".
app
  :: (Typeable env, CmdCommon env, HasJobEnv' env, HasNLPServer env, HasSettings env)
  => AuthenticatedUser
  -> AccessPolicyManager
  -> App (EVENT (GargM env BackendInternalError)) (GargM env BackendInternalError)
app authenticatedUser policyManager = deriveApp (rootResolver authenticatedUser policyManager)

----------------------------------------------

-- Now for some boilerplate to integrate the above GraphQL app with
-- servant.

-- | Servant route for the app we defined above.
type GQAPI = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse
-- type Schema = "schema" :> Get '[PlainText] Text
-- | Servant route for the playground.
type Playground = Get '[HTML] ByteString
-- type API' (name :: Symbol) = name :> (GQAPI :<|> Schema :<|> Playground)
-- | Our API consists of `GQAPI` and `Playground`.
type API = SA.Auth '[SA.JWT, SA.Cookie] AuthenticatedUser
            :> "gql" :> (PolicyChecked GQAPI :<|> Playground)

gqapi :: Proxy API
gqapi = Proxy

-- serveEndpoint ::
--   ( SubApp ServerApp e
--   , PubApp e
--   ) =>
--   [e -> IO ()] ->
--   App e IO ->
--   Server (API name)
-- serveEndpoint publish app' = (liftIO . httpPubApp publish app') :<|> withSchema app' :<|> pure httpPlayground
--
-- withSchema :: (Applicative f) => App e m -> f Text
-- withSchema = pure . LT.toStrict . decodeUtf8 . render

-- | Implementation of our API.
--api :: Server API
api
  :: (Typeable env, CmdCommon env, HasJobEnv' env, HasSettings env)
  => ServerT API (GargM env BackendInternalError)
api (SAS.Authenticated auser) = (httpPubApp [] . app auser) :<|> pure httpPlayground
api _                         = panic "401 in graphql" -- SAS.throwAll (_ServerError # err401)
