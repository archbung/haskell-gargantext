{-|
Module      : Gargantext.Database.Types
Description : Specific Types to manage core Gargantext type with database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Types
  where

import Database.PostgreSQL.Simple qualified as PGS
import Gargantext.Core.Text (HasText(..))
import Gargantext.Database.Schema.Prelude
import Gargantext.Prelude


-- | Index memory of any type in Gargantext
data Indexed i a =
  Indexed { _index     :: !i
          , _unIndex   :: !a
          }
  deriving (Show, Generic, Eq, Ord)

makeLenses ''Indexed

instance Bifunctor Indexed where
  first  f (Indexed i a) = Indexed (f i) a
  second g (Indexed i a) = Indexed i (g a)

----------------------------------------------------------------------
-- | Main instances
instance (FromField i, FromField a) => PGS.FromRow (Indexed i a) where
  fromRow = Indexed <$> field <*> field

instance HasText a => HasText (Indexed i a)
  where
    hasText (Indexed _ a) = hasText a

instance (Hashable a, Hashable b) => Hashable (Indexed a b)

instance DefaultFromField (Nullable SqlInt4)   Int            where
    defaultFromField = fromPGSFromField

instance DefaultFromField (Nullable SqlFloat8) Int            where
    defaultFromField = fromPGSFromField

instance DefaultFromField (Nullable SqlFloat8) Double         where
    defaultFromField = fromPGSFromField

instance DefaultFromField SqlFloat8            (Maybe Double) where
    defaultFromField = fromPGSFromField

instance DefaultFromField SqlInt4              (Maybe Int)    where
    defaultFromField = fromPGSFromField

instance DefaultFromField (Nullable SqlText)  Text  where
    defaultFromField = fromPGSFromField
