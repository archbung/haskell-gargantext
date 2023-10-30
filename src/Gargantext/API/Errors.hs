{-# LANGUAGE LambdaCase #-}

module Gargantext.API.Errors (
  module Types

  -- * Conversion functions
  , backendErrorTypeToErrStatus
  ) where

import Gargantext.API.Errors.Types as Types
import qualified Network.HTTP.Types.Status as HTTP

backendErrorTypeToErrStatus :: BackendErrorType -> HTTP.Status
backendErrorTypeToErrStatus = \case
  BE_phylo_corpus_not_ready    -> HTTP.status500
  BE_node_not_found            -> HTTP.status500
  BE_tree_error_root_not_found -> HTTP.status404
