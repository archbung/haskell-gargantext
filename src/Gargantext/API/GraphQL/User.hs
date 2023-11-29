{-|
Module      : Gargantext.API.GraphQL.User
Description :
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.User where

import Data.Morpheus.Types ( GQLType )
import Gargantext.API.Admin.Auth.Types
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Auth.PolicyCheck
import Gargantext.API.GraphQL.PolicyCheck
import Gargantext.API.GraphQL.Types
import Gargantext.Core.Types.Individu qualified as Individu
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataUser(..))
import Gargantext.Database.Prelude (CmdCommon)
import Gargantext.Database.Query.Table.User qualified as DBUser
import Gargantext.Database.Schema.User (UserLight(..))
import Gargantext.Prelude
import Gargantext.Core.Types

data User m = User
  { u_email     :: Text
  , u_hyperdata :: m (Maybe HyperdataUser)
  , u_id        :: UserId
  , u_username  :: Text }
  deriving (Generic, GQLType)

-- | Arguments to the "user" query.
data UserArgs
  = UserArgs
    { user_id :: Int
    } deriving (Generic, GQLType)

data UserPubmedAPIKeyMArgs
  = UserPubmedAPIKeyMArgs
    { user_id :: Int
    , api_key  :: Text }
    deriving (Generic, GQLType)

data UserEPOAPIUserMArgs
  = UserEPOAPIUserMArgs
    { user_id  :: Int
    , api_user :: Text }
    deriving (Generic, GQLType)

data UserEPOAPITokenMArgs
  = UserEPOAPITokenMArgs
    { user_id   :: Int
    , api_token :: Text }
    deriving (Generic, GQLType)

-- | Function to resolve user from a query.
resolveUsers
  :: (CmdCommon env)
  => AuthenticatedUser
  -> AccessPolicyManager
  -> UserArgs
  -> GqlM e env [User (GqlM e env)]
resolveUsers autUser mgr UserArgs { user_id } = do
  -- We are given the /node id/ of the logged-in user.
  withPolicy autUser mgr (nodeChecks $ UnsafeMkNodeId user_id) $ dbUsers user_id

-- | Inner function to fetch the user from DB.
dbUsers :: (CmdCommon env)
        => Int -> GqlM e env [User (GqlM e env)]
dbUsers user_id = lift (map toUser <$> DBUser.getUsersWithId (Individu.RootId $ UnsafeMkNodeId user_id))

toUser
  :: (CmdCommon env)
  => UserLight -> User (GqlM e env)
toUser (UserLight { .. }) = User { u_email = userLight_email
                                 , u_hyperdata = resolveHyperdata userLight_id
                                 , u_id = userLight_id
                                 , u_username = userLight_username }

resolveHyperdata
  :: (CmdCommon env)
  => UserId -> GqlM e env (Maybe HyperdataUser)
resolveHyperdata userid = lift (listToMaybe <$> DBUser.getUserHyperdata (Individu.UserDBId userid))

updateUserPubmedAPIKey :: ( CmdCommon env, HasSettings env) =>
                          UserPubmedAPIKeyMArgs -> GqlM' e env Int
updateUserPubmedAPIKey UserPubmedAPIKeyMArgs { user_id, api_key } = do
  _ <- lift $ DBUser.updateUserPubmedAPIKey (Individu.RootId $ UnsafeMkNodeId user_id) api_key

  pure 1

updateUserEPOAPIUser :: ( CmdCommon env, HasSettings env) =>
                        UserEPOAPIUserMArgs -> GqlM' e env Int
updateUserEPOAPIUser UserEPOAPIUserMArgs { user_id, api_user } = do
  _ <- lift $ DBUser.updateUserEPOAPIUser (Individu.RootId $ UnsafeMkNodeId user_id) api_user

  pure 1

updateUserEPOAPIToken :: ( CmdCommon env, HasSettings env) =>
                         UserEPOAPITokenMArgs -> GqlM' e env Int
updateUserEPOAPIToken UserEPOAPITokenMArgs { user_id, api_token } = do
  _ <- lift $ DBUser.updateUserEPOAPIToken (Individu.RootId $ UnsafeMkNodeId user_id) api_token

  pure 1
