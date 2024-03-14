{-|
Module      : Gargantext.Core.NodeStory.Types
Description : Node API generation
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Core.NodeStory.Types
  ( HasNodeStory
  , HasNodeStoryEnv
  , hasNodeStory
  , HasNodeStoryImmediateSaver
  , hasNodeStoryImmediateSaver
  , HasNodeArchiveStoryImmediateSaver
  , hasNodeArchiveStoryImmediateSaver
  , NodeStory(..)
  , NgramsState'
  , NgramsStatePatch'
  , NodeListStory
  , ArchiveList
  , initNodeListStoryMock
  , NodeStoryEnv(..)
  , initNodeStory
  , nse_getter
  , nse_getter_multi
  , nse_saver_immediate
  , nse_archive_saver_immediate
  -- , nse_var
  , unNodeStory
  , Archive(..)
  , initArchive
  , archiveAdvance
  , unionArchives
  , a_history
  , a_state
  , a_version
  , combineState
  , ArchiveStateSet
  , ArchiveStateList )
where

import Codec.Serialise.Class ( Serialise )
import Control.Lens (makeLenses, Getter, (^.))
import Data.Aeson hiding ((.=), decode)
import Data.Map.Strict qualified as Map
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Set qualified as Set
import Database.PostgreSQL.Simple.FromField (FromField(fromField), fromJSONField)
import Gargantext.API.Ngrams.Types
import Gargantext.Database.Admin.Types.Node ( NodeId(..) )
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Admin.Config ()
import Gargantext.Database.Prelude (DbCmd')
import Gargantext.Database.Schema.Ngrams qualified as TableNgrams
import Gargantext.Database.Query.Table.Node.Error (HasNodeError())
import Gargantext.Prelude hiding (to)
import Opaleye (DefaultFromField(..), SqlJsonb, fromPGSFromField)


------------------------------------------------------------------------

{- | Node Story for each NodeType where the Key of the Map is NodeId
  TODO : generalize for any NodeType, let's start with NodeList which
  is implemented already
-}
newtype NodeStory s p = NodeStory { _unNodeStory :: Map NodeId (Archive s p) }
  deriving (Generic, Show, Eq)

instance (FromJSON s, FromJSON p) => FromJSON (NodeStory s p)
instance (ToJSON s, ToJSON p) => ToJSON (NodeStory s p)
instance (Serialise s, Serialise p) => Serialise (NodeStory s p)

data Archive s p = Archive
  { _a_version           :: !Version
  , _a_state             :: !s
  , _a_history           :: ![p]
    -- first patch in the list is the most recent
    -- We use `take` in `commitStatePatch`, that's why.

    -- History is immutable, we just insert things on top of existing
    -- list.

    -- We don't need to store the whole history in memory, this
    -- structure holds only recent history, the one that will be
    -- inserted to the DB.
  }
  deriving (Generic, Show, Eq)

instance (Serialise s, Serialise p) => Serialise (Archive s p)


type NodeListStory     = NodeStory NgramsState' NgramsStatePatch'

-- NOTE: 'type NgramsTableMap = Map NgramsTerm NgramsRepoElement'
type NgramsState'      = Map       TableNgrams.NgramsType NgramsTableMap
type NgramsStatePatch' = PatchMap  TableNgrams.NgramsType NgramsTablePatch
-- instance Serialise NgramsStatePatch'
instance FromField (Archive NgramsState' NgramsStatePatch')
  where
    fromField = fromJSONField
instance DefaultFromField SqlJsonb (Archive NgramsState' NgramsStatePatch')
  where
    defaultFromField = fromPGSFromField


-- | Combine `NgramsState'`. This is because the structure is (Map
-- NgramsType (Map ...)) and the default `(<>)` operator is
-- left-biased
-- (https://hackage.haskell.org/package/containers-0.6.6/docs/Data-Map-Internal.html#v:union)
combineState :: NgramsState' -> NgramsState' -> NgramsState'
combineState = Map.unionWith (<>)

-- This is not a typical Semigroup instance. The state is not
-- appended, instead it is replaced with the second entry. This is
-- because state changes with each version. We have to take into
-- account the removal of terms as well.
-- instance (Semigroup s, Semigroup p) => Semigroup (Archive s p) where
--   (<>) (Archive { _a_history = p }) (Archive { _a_version = v'
--                                              , _a_state = s'
--                                              , _a_history = p' }) =
--     Archive { _a_version = v'
--             , _a_state = s'
--             , _a_history = p' <> p }
-- instance (Monoid s, Semigroup p) => Monoid (Archive s p) where
--   mempty = Archive { _a_version = 0
--                    , _a_state = mempty
--                    , _a_history = [] }
instance (FromJSON s, FromJSON p) => FromJSON (Archive s p) where
  parseJSON = genericParseJSON $ unPrefix "_a_"
instance (ToJSON s, ToJSON p) => ToJSON (Archive s p) where
  toJSON     = genericToJSON     $ unPrefix "_a_"
  toEncoding = genericToEncoding $ unPrefix "_a_"

-- | This is the normal way to update archive state, bumping the
-- version and history. Resulting state is taken directly from new
-- archive, omitting old archive completely.
archiveAdvance :: (Semigroup s, Semigroup p) => Archive s p -> Archive s p -> Archive s p
archiveAdvance aOld aNew = aNew { _a_history = _a_history aNew <> _a_history aOld }

-- | This is to merge archive states.
unionArchives :: (Semigroup s, Semigroup p) => Archive s p -> Archive s p -> Archive s p
unionArchives aOld aNew = aNew { _a_state = _a_state aOld <> _a_state aNew
                               , _a_history = _a_history aNew <> _a_history aOld }


------------------------------------------------------------------------
initNodeStory :: (Monoid s, Semigroup p) => NodeId -> NodeStory s p
initNodeStory ni = NodeStory $ Map.singleton ni initArchive

initArchive :: (Monoid s, Semigroup p) => Archive s p
initArchive = Archive { _a_version = 0
                      , _a_state = mempty
                      , _a_history = [] }

initNodeListStoryMock :: NodeListStory
initNodeListStoryMock = NodeStory $ Map.singleton nodeListId archive
  where
    nodeListId = 0
    archive = Archive { _a_version = 0
                      , _a_state = ngramsTableMap
                      , _a_history = [] }
    ngramsTableMap = Map.singleton TableNgrams.NgramsTerms
                   $ Map.fromList
                   [ (n ^. ne_ngrams, ngramsElementToRepo n)
                   | n <- mockTable ^. _NgramsTable
                   ]

----------------------------------------------------------------------
data NodeStoryPoly nid v ngtid ngid nre =
  NodeStoryDB { node_id             :: !nid
              , version             :: !v
              , ngrams_type_id      :: !ngtid
              , ngrams_id           :: !ngid
              , ngrams_repo_element :: !nre }
  deriving (Eq)

data NodeStoryArchivePoly nid a =
  NodeStoryArchiveDB { a_node_id :: !nid
                     , archive   :: !a }
  deriving (Eq)

$(makeAdaptorAndInstance "pNodeStory" ''NodeStoryPoly)
$(makeAdaptorAndInstance "pNodeArchiveStory" ''NodeStoryArchivePoly)

-- type NodeStoryWrite = NodeStoryPoly (Column SqlInt4) (Column SqlInt4) (Column SqlInt4) (Column SqlInt4) (Column SqlJsonb)
-- type NodeStoryRead = NodeStoryPoly (Column SqlInt4) (Column SqlInt4) (Column SqlInt4) (Column SqlInt4) (Column SqlJsonb)

-- type NodeStoryArchiveWrite = NodeStoryArchivePoly (Column SqlInt4) (Column SqlJsonb)
-- type NodeStoryArchiveRead = NodeStoryArchivePoly (Column SqlInt4) (Column SqlJsonb)

type ArchiveList = Archive NgramsState' NgramsStatePatch'



------------------------------------------------------------------------
data NodeStoryEnv = NodeStoryEnv
  { _nse_saver_immediate :: !(NodeId -> ArchiveList -> IO ())
  , _nse_archive_saver_immediate :: !(NodeId -> ArchiveList -> IO ArchiveList)
  , _nse_getter :: !(NodeId -> IO ArchiveList)
  , _nse_getter_multi :: !([NodeId] -> IO NodeListStory)
  --, _nse_cleaner :: !(IO ()) -- every 12 hours: cleans the repos of unused NodeStories
  -- , _nse_lock  :: !FileLock -- TODO (it depends on the option: if with database or file only)
  }
  deriving (Generic)

type HasNodeStory env err m = ( DbCmd' env err m
                              , MonadReader env m
                              , MonadError  err m
                              , HasNodeStoryEnv env
                              , HasNodeError err
                              )

class (HasNodeStoryImmediateSaver env)
  => HasNodeStoryEnv env where
    hasNodeStory :: Getter env NodeStoryEnv

class HasNodeStoryImmediateSaver env where
  hasNodeStoryImmediateSaver :: Getter env (NodeId -> ArchiveList -> IO ())

class HasNodeArchiveStoryImmediateSaver env where
  hasNodeArchiveStoryImmediateSaver :: Getter env (NodeId -> ArchiveList -> IO ArchiveList)




type ArchiveStateList = [(TableNgrams.NgramsType, NgramsTerm, NgramsRepoElement)]
type ArchiveStateSet = Set.Set (TableNgrams.NgramsType, NgramsTerm)

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Lenses at the bottom of the file because Template Haskell would reorder order of execution in others cases
makeLenses ''NodeStoryEnv
makeLenses ''NodeStory
makeLenses ''Archive
