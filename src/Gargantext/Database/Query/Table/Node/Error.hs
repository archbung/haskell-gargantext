{-# LANGUAGE LambdaCase #-}
{-|
Module      : Gargantext.Database.Types.Error
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Database.Query.Table.Node.Error (
  -- * Types
    NodeError(..)
  , NodeCreationError(..)
  , NodeLookupError(..)

  -- * Classes
  , HasNodeError(..)

  -- * Functions
  , errorWith
  , nodeError
  , nodeCreationError
  , nodeLookupError
  , catchNodeError
  ) where

import Control.Lens (Prism', (#), (^?))
import Data.Aeson
import Data.Text qualified as T
import Gargantext.Core.Types.Individu
import Gargantext.Database.Admin.Types.Node (ListId, NodeId(..), ContextId, UserId, ParentId)
import Gargantext.Prelude hiding (sum, head)
import Prelude hiding (null, id, map, sum, show)
import Prelude qualified

data NodeCreationError
  = UserParentAlreadyExists UserId ParentId
  | UserParentDoesNotExist  UserId
  | UserHasNegativeId       UserId
  | InsertNodeFailed        UserId ParentId
  deriving (Show, Eq, Generic)

instance ToJSON NodeCreationError

renderNodeCreationFailed :: NodeCreationError -> T.Text
renderNodeCreationFailed = \case
  UserParentAlreadyExists uid pId -> "user id " <> T.pack (show uid) <> " has already a parent: " <> T.pack (show pId)
  UserParentDoesNotExist uid -> "user id " <> T.pack (show uid) <> " has no parent"
  UserHasNegativeId uid      -> "user id " <> T.pack (show uid) <> " is a negative id."
  InsertNodeFailed uid pid -> "couldn't create the list for user id " <> T.pack (show uid) <> " and parent id " <> T.pack (show pid)

data NodeLookupError
  = NodeDoesNotExist     NodeId
  | NodeParentDoesNotExist NodeId
  | UserDoesNotExist     UserId
  | UserNameDoesNotExist Username
  | UserHasTooManyRoots UserId [NodeId]
  deriving (Show, Eq, Generic)

instance ToJSON NodeLookupError

renderNodeLookupFailed :: NodeLookupError -> T.Text
renderNodeLookupFailed = \case
  NodeDoesNotExist nid   -> "node with id " <> T.pack (show nid) <> " couldn't be found."
  NodeParentDoesNotExist nid -> "no parent for node with id " <> T.pack (show nid) <> "."
  UserDoesNotExist uid -> "user with id " <> T.pack (show uid) <> " couldn't be found."
  UserNameDoesNotExist uname -> "user with username '" <> uname <> "' couldn't be found."
  UserHasTooManyRoots uid roots -> "user with id " <> T.pack (show uid) <> " has too many roots: [" <> T.intercalate "," (map (T.pack . show) roots)

------------------------------------------------------------------------
data NodeError = NoListFound ListId
               | NoRootFound
               | NoCorpusFound
               | NoUserFound User
               | NodeCreationFailed NodeCreationError
               | NodeLookupFailed   NodeLookupError
               | NotImplYet
               | NoContextFound ContextId
               | NeedsConfiguration
               | NodeError SomeException
               -- Left for backward compatibility, but we should remove them.
               | DoesNotExist NodeId

instance Prelude.Show NodeError
  where
    show (NoListFound {})   = "No list found"
    show NoRootFound   = "No root found"
    show NoCorpusFound = "No corpus found"
    show (NoUserFound ur) = "User(" <> T.unpack (renderUser ur) <> ") not found"

    show (NodeCreationFailed reason) = "Cannot make node due to: " <> T.unpack (renderNodeCreationFailed reason)
    show NotImplYet    = "Not implemented yet"
    show (NodeLookupFailed reason) = "Cannot lookup node due to: " <> T.unpack (renderNodeLookupFailed reason)
    show (NoContextFound n) = "Context node does not exist (" <> show n <> ")"
    show NeedsConfiguration = "Needs configuration"
    show (NodeError e)      = "NodeError: " <> displayException e
    show (DoesNotExist n)   = "Node does not exist (" <> show n <> ")"

instance ToJSON NodeError where
  toJSON (DoesNotExist n) =
    object [ ( "error", "Node does not exist" )
           , ( "node", toJSON n ) ]
  toJSON (NoListFound listId) =
    object [ ( "error", "No list found" )
           , ( "listId", toJSON listId ) ]
  toJSON (NodeError e) =
    object [ ( "error", "Node error" )
           , ( "exception", toJSON $ T.pack $ show e ) ]
  toJSON (NoUserFound ur) =
    object [ ( "error", "No user found" )
           , ( "user", toJSON ur ) ]
  toJSON (NodeCreationFailed reason) =
    object [ ( "error", "Node creation failed" )
           , ( "reason", toJSON reason ) ]
  toJSON (NodeLookupFailed reason) =
    object [ ( "error", "Node lookup failed" )
           , ( "reason", toJSON reason ) ]
  toJSON (NoContextFound n) =
    object [ ( "error", "No context found" )
           , ( "node", toJSON n ) ]
  toJSON err =
    object [ ( "error", toJSON $ T.pack $ show err ) ]

class HasNodeError e where
  _NodeError :: Prism' e NodeError

errorWith :: ( MonadError e m
            , HasNodeError e)
          => Text -> m a
errorWith x = nodeError (NodeError $ toException $ userError $ T.unpack x)

nodeError :: ( MonadError e m
             , HasNodeError e)
          => NodeError -> m a
nodeError ne = throwError $ _NodeError # ne

nodeCreationError :: ( MonadError e m, HasNodeError e)
                  => NodeCreationError
                  -> m a
nodeCreationError ne = throwError $ _NodeError # NodeCreationFailed ne

nodeLookupError :: ( MonadError e m, HasNodeError e)
                => NodeLookupError
                -> m a
nodeLookupError ne = throwError $ _NodeError # NodeLookupFailed ne

catchNodeError :: (MonadError e m, HasNodeError e) => m a -> (NodeError -> m a) -> m a
catchNodeError f g = catchError f (\e -> maybe (throwError e) g (e ^? _NodeError))
