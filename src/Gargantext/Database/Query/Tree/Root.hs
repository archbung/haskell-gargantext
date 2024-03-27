{-|
Module      : Gargantext.Database.Root
Description : Main requests to get root of users
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE Arrows #-}

module Gargantext.Database.Query.Tree.Root
  where

import Control.Arrow (returnA)
import Gargantext.Core (HasDBid(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Types.Main (CorpusName)
import Gargantext.Database.Action.Node ( mkNodeWithParent )
import Gargantext.Database.Action.User (getUserId, getUsername)
import Gargantext.Database.Admin.Config ( corpusMasterName, userMaster )
import Gargantext.Database.Admin.Types.Hyperdata.User ( HyperdataUser )
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (runOpaQuery, DBCmd)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Query.Table.User (queryUserTable, UserPoly(..))
import Gargantext.Database.Schema.Node (NodePoly(..), NodeRead, queryNodeTable)
import Gargantext.Prelude
import Opaleye (restrict, (.==), Select)
import Opaleye.SqlTypes (sqlStrictText, sqlInt4)


getRootId :: (HasNodeError err) => User -> DBCmd err NodeId
getRootId u = do
  maybeRoot <- head <$> getRoot u
  case maybeRoot of
    Nothing -> errorWith "[G.D.Q.T.R.getRootId] No root id"
    Just  r -> pure (_node_id r)

getRoot :: User -> DBCmd err [Node HyperdataUser]
getRoot = runOpaQuery . selectRoot

getOrMkRoot :: (HasNodeError err)
            => User
            -> DBCmd err (UserId, RootId)
getOrMkRoot user = do
  userId <- getUserId user

  rootId' <- map _node_id <$> getRoot user

  rootId'' <- case rootId' of
        []  -> mkRoot user
        n   -> case length n >= 2 of
            True  -> nodeError $ NodeLookupFailed $ UserHasTooManyRoots userId n
            False -> pure rootId'

  rootId <- maybe (nodeError NoRootFound) pure (head rootId'')
  pure (userId, rootId)


-- | Datatype for the `getOrMkRootWithCorpus`.
-- There are only 3 possibilities:
-- - User is userMaster and then there is no corpus name
-- - User is a normal user and then we pass corpus name
-- - User is a normal user and then we pass corpus ids
data MkCorpusUser =
    MkCorpusUserMaster
  | MkCorpusUserNormalCorpusName User CorpusName
  | MkCorpusUserNormalCorpusIds  User [CorpusId]
  deriving (Eq, Show)

userFromMkCorpusUser :: MkCorpusUser -> User
userFromMkCorpusUser MkCorpusUserMaster = UserName userMaster
userFromMkCorpusUser (MkCorpusUserNormalCorpusIds u _cids) = u
userFromMkCorpusUser (MkCorpusUserNormalCorpusName u _cname) = u


getOrMkRootWithCorpus :: (HasNodeError err, MkCorpus a)
                      => MkCorpusUser
                      -> Maybe a
                      -> DBCmd err (UserId, RootId, CorpusId)
getOrMkRootWithCorpus MkCorpusUserMaster c = do
  (userId, rootId) <- getOrMkRoot (UserName userMaster)
  corpusId'' <- do
    ns <- getCorporaWithParentId rootId
    pure $ map _node_id ns
  case corpusId'' of
    []   -> mkCorpus corpusMasterName c rootId userId
    cIds -> do
      corpusId <- maybe (nodeError NoCorpusFound) pure (head cIds)
      pure (userId, rootId, corpusId)
getOrMkRootWithCorpus (MkCorpusUserNormalCorpusName user cName) c = do
  (userId, rootId) <- getOrMkRoot user
  mkCorpus cName c rootId userId
getOrMkRootWithCorpus (MkCorpusUserNormalCorpusIds user []) c = do
  getOrMkRootWithCorpus (MkCorpusUserNormalCorpusName user "Default") c
getOrMkRootWithCorpus (MkCorpusUserNormalCorpusIds user cIds) _c = do
  (userId, rootId) <- getOrMkRoot user
  corpusId <- maybe (nodeError NoCorpusFound) pure (head cIds)
  pure (userId, rootId, corpusId)


-- | Helper function for `getOrMkRootWithCorpus`.
mkCorpus :: (HasNodeError err, MkCorpus a)
         => CorpusName
         -> Maybe a
         -> RootId
         -> UserId
         -> DBCmd err (UserId, RootId, CorpusId)
mkCorpus cName c rootId userId = do
  c' <- mk (Just cName) c rootId userId
  _tId <- case head c' of
            Nothing  -> errorWith "[G.D.Q.T.Root.getOrMk...] mk Corpus failed"
            Just c'' -> insertDefaultNode NodeTexts c'' userId

  corpusId <- maybe (nodeError NoCorpusFound) pure (head c')
  pure (userId, rootId, corpusId)


mkRoot :: HasNodeError err
       => User
       -> DBCmd err [RootId]
mkRoot user = do

  -- TODO
  -- udb <- getUserDb user
  -- let uid = user_id udb
  uid <- getUserId user

  -- TODO ? Which name for user Node ?
  una <- getUsername user

  case isPositive uid of
     False -> nodeError $ NodeCreationFailed (UserHasNegativeId uid)
     True  -> do
       rs <- mkNodeWithParent NodeUser Nothing uid una
       _ <- case rs of
         [r] -> do
           _ <- insertNode NodeFolderPrivate Nothing Nothing r uid
           _ <- insertNode NodeFolderShared Nothing Nothing r uid
           _ <- insertNode NodeFolderPublic Nothing Nothing r uid
           pure rs
         _   -> pure rs
       pure rs

selectRoot :: User -> Select NodeRead
selectRoot (UserName username) = proc () -> do
    row   <- queryNodeTable -< ()
    users <- queryUserTable -< ()
    restrict -< _node_typename row   .== sqlInt4 (toDBid NodeUser)
    restrict -< user_username  users .== sqlStrictText username
    restrict -< _node_user_id   row   .== user_id users
    returnA  -< row

selectRoot (UserDBId uid) = proc () -> do
    row   <- queryNodeTable -< ()
    restrict -< _node_typename row   .== sqlInt4 (toDBid NodeUser)
    restrict -< _node_user_id   row   .== sqlInt4 (_UserId uid)
    returnA  -< row

selectRoot (RootId nid) =
 proc () -> do
    row   <- queryNodeTable -< ()
    restrict -< _node_typename row   .== sqlInt4 (toDBid NodeUser)
    restrict -< _node_id   row   .== pgNodeId nid
    returnA  -< row
