{-|
Module      : Gargantext.Database.Types.Error
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Database.Query.Table.Node.Error where

import Control.Lens (Prism', (#), (^?))
import Data.Aeson
import Data.Text qualified as T
import Gargantext.Core.Types.Individu

import Prelude hiding (null, id, map, sum, show)

import Gargantext.Database.Admin.Types.Node (ListId, NodeId(..), ContextId)
import Gargantext.Prelude hiding (sum, head)
import Prelude qualified

------------------------------------------------------------------------
data NodeError = NoListFound { listId :: ListId }
               | NoRootFound
               | NoCorpusFound
               | NoUserFound User
               | MkNode
               | UserNoParent
               | HasParent
               | ManyParents
               | NegativeId
               | NotImplYet
               | ManyNodeUsers
               | DoesNotExist NodeId
               | NoContextFound ContextId
               | NeedsConfiguration
               | NodeError Text
               | QueryNoParse Text

instance Prelude.Show NodeError
  where
    show (NoListFound {})   = "No list   found"
    show NoRootFound   = "No Root   found"
    show NoCorpusFound = "No Corpus found"
    show (NoUserFound ur) = "User(" <> T.unpack (renderUser ur) <> ") not found"

    show MkNode        = "Cannot make node"
    show NegativeId    = "Node with negative Id"
    show UserNoParent  = "Should not have parent"
    show HasParent     = "NodeType has parent"
    show NotImplYet    = "Not implemented yet"
    show ManyParents   = "Too many parents"
    show ManyNodeUsers = "Many userNode/user"
    show (DoesNotExist n)   = "Node does not exist (" <> show n <> ")"
    show (NoContextFound n) = "Context node does not exist (" <> show n <> ")"
    show NeedsConfiguration = "Needs configuration"
    show (NodeError e)      = "NodeError: " <> cs e
    show (QueryNoParse err) = "QueryNoParse: " <> T.unpack err

instance ToJSON NodeError where
  toJSON (NoListFound { listId }) =
    object [ ( "error", "No list found" )
           , ( "listId", toJSON listId ) ]
  toJSON err =
    object [ ( "error", String $ T.pack $ show err ) ]

class HasNodeError e where
  _NodeError :: Prism' e NodeError

errorWith :: ( MonadError e m
            , HasNodeError e)
          => Text -> m a
errorWith x = nodeError (NodeError x)

nodeError :: ( MonadError e m
             , HasNodeError e)
          => NodeError -> m a
nodeError ne = throwError $ _NodeError # ne

catchNodeError :: (MonadError e m, HasNodeError e) => m a -> (NodeError -> m a) -> m a
catchNodeError f g = catchError f (\e -> maybe (throwError e) g (e ^? _NodeError))
