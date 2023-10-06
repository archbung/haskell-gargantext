{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.User where

import Data.Maybe (listToMaybe)
import Data.Morpheus.Types
  ( GQLType
  , lift
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.GraphQL.Types
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataUser(..))
import Gargantext.Database.Admin.Types.Node (NodeId(..))
import Gargantext.Database.Prelude (CmdCommon)
import Gargantext.Database.Schema.User (UserLight(..))
import Gargantext.Prelude
import qualified Gargantext.Core.Types.Individu as Individu
import qualified Gargantext.Database.Query.Table.User as DBUser
import Gargantext.API.Admin.Auth.Types
import Gargantext.API.Auth.PolicyCheck
import Gargantext.API.GraphQL.PolicyCheck

data User m = User
  { u_email     :: Text
  , u_hyperdata :: m (Maybe HyperdataUser)
  , u_id        :: Int
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

-- | Function to resolve user from a query.
resolveUsers
  :: (CmdCommon env)
  => AuthenticatedUser
  -> AccessPolicyManager
  -> UserArgs
  -> GqlM e env [User (GqlM e env)]
resolveUsers autUser mgr UserArgs { user_id } = do
  -- FIXME(adn) we should use a proper policy, not 'alwaysAllow'.
  withPolicy autUser mgr alwaysAllow $ dbUsers user_id

-- | Inner function to fetch the user from DB.
dbUsers
  :: (CmdCommon env)
  => Int -> GqlM e env [User (GqlM e env)]
dbUsers user_id = lift (map toUser <$> DBUser.getUsersWithId (Individu.RootId $ NodeId user_id))

toUser
  :: (CmdCommon env)
  => UserLight -> User (GqlM e env)
toUser (UserLight { .. }) = User { u_email = userLight_email
                                 , u_hyperdata = resolveHyperdata userLight_id
                                 , u_id = userLight_id
                                 , u_username = userLight_username }

resolveHyperdata
  :: (CmdCommon env)
  => Int -> GqlM e env (Maybe HyperdataUser)
resolveHyperdata userid = lift (listToMaybe <$> DBUser.getUserHyperdata (Individu.UserDBId userid))

updateUserPubmedAPIKey :: ( CmdCommon env, HasSettings env) =>
                          UserPubmedAPIKeyMArgs -> GqlM' e env Int
updateUserPubmedAPIKey UserPubmedAPIKeyMArgs { user_id, api_key } = do
  _ <- lift $ DBUser.updateUserPubmedAPIKey (Individu.RootId $ NodeId user_id) api_key

  pure 1
