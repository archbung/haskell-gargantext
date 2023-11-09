{-|
Module      : Gargantext.Database.Tree.Error
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE QuasiQuotes       #-}

module Gargantext.Database.Query.Tree.Error
  where

import Control.Lens (Prism', (#))
import Gargantext.Core.Types
import Gargantext.Prelude
import Prelude qualified
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

------------------------------------------------------------------------
data TreeError = NoRoot
               | EmptyRoot
               | TooManyRoots (NonEmpty NodeId)

instance Prelude.Show TreeError
  where
    show NoRoot               = "Root node not found"
    show EmptyRoot            = "Root node should not be empty"
    show (TooManyRoots roots) = "Too many root nodes: [" <> T.unpack (T.intercalate "," . map show $ NE.toList roots) <> "]"

class HasTreeError e where
  _TreeError :: Prism' e TreeError

treeError :: ( MonadError e m
             , HasTreeError e )
             => TreeError
             -> m a
treeError te = throwError $ _TreeError # te

