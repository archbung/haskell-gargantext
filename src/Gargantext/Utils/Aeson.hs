{-|
Module      : Gargantext.Utils.Aeson
Description : Gargantext utilities
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Utilities for handling zip files
-}

module Gargantext.Utils.Aeson where

import Data.Aeson.Types

-- this is what purescript Simple.JSON generics assumes
defaultTaggedObject :: SumEncoding
defaultTaggedObject = TaggedObject { tagFieldName = "type", contentsFieldName = "value" }
