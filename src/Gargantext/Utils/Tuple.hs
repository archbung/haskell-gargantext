
{-|
Module      : Gargantext.Utils.Tuple
Description : Gargantext utilities
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Utils.Tuple where

import Protolude



uncurryMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
uncurryMaybe (Nothing, _) = Nothing
uncurryMaybe (_, Nothing) = Nothing
uncurryMaybe (Just a, Just b) = Just (a, b)

uncurryMaybeSecond :: (a, Maybe b) -> Maybe (a, b)
uncurryMaybeSecond (_, Nothing) = Nothing
uncurryMaybeSecond (a, Just b) = Just (a, b)
