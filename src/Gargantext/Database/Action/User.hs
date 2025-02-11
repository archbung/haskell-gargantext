{-|
Module      : Gargantext.Database.Action.User
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Database.Action.User
    where

import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (DBCmd)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.User
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Schema.Node
import Gargantext.Prelude

------------------------------------------------------------------------
getUserLightWithId :: HasNodeError err => UserId -> DBCmd err UserLight
getUserLightWithId i = do
  candidates <- head <$> getUsersWithId (UserDBId i)
  case candidates of
    Nothing -> nodeError (NodeLookupFailed $ UserDoesNotExist i)
    Just u  -> pure u

getUserLightDB :: HasNodeError err => User -> DBCmd err UserLight
getUserLightDB u = do
  userId <- getUserId u
  userLight <- getUserLightWithId userId
  pure userLight

------------------------------------------------------------------------
getUserId :: HasNodeError err
          => User
          -> DBCmd err UserId
getUserId u = do
  maybeUser <- getUserId' u
  case maybeUser of
    Left reason -> nodeError $ NodeLookupFailed reason
    Right u'    -> pure u'

getUserId' :: HasNodeError err
          => User
          -> DBCmd err (Either NodeLookupError UserId)
getUserId' (UserDBId uid) = pure (Right uid)
getUserId' (RootId   rid) = do
  n <- getNode rid
  pure $ Right $ _node_user_id n
getUserId' (UserName u  ) = do
  muser <- getUser u
  case muser of
    Just user -> pure $ Right $ userLight_id user
    Nothing   -> pure $ Left $ UserNameDoesNotExist u

------------------------------------------------------------------------
-- | Username = Text
-- UserName is User
-- that is confusing, we should change this
type Username = Text
getUsername :: HasNodeError err
            => User
            -> DBCmd err Username
getUsername (UserName u) = pure u
getUsername user@(UserDBId _) = do
  users <- getUsersWithId user
  case head users of
    Just u  -> pure $ userLight_username u
    Nothing -> errorWith "G.D.A.U.getUserName: User not found with that id"
getUsername (RootId   rid) = do
  n <- getNode rid
  getUsername (UserDBId $ _node_user_id n)

--------------------------------------------------------------------------
-- getRootId is in Gargantext.Database.Query.Tree.Root
