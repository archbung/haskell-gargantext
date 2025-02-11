{-|
Module      : Gargantext.API.Context
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}

module Gargantext.API.Context
  where

import Prelude

import Data.Aeson (FromJSON, ToJSON)
import Servant

import Gargantext.API.Admin.Auth (withAccess)
import Gargantext.API.Admin.Auth.Types (PathId(..), AuthenticatedUser)
import Gargantext.API.Prelude
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (JSONB)
import Gargantext.Database.Query.Table.Context

-------------------------------------------------------------------
-- TODO use Context instead of Node
type ContextAPI a = Get '[JSON] (Node a)

------------------------------------------------------------------------
-- TODO NodeAPI -> ContextAPI
contextAPI :: forall proxy a.
       ( JSONB a
       , FromJSON a
       , ToJSON a
       ) => proxy a
         -> AuthenticatedUser
         -> ContextId
         -> GargServer (ContextAPI a)
contextAPI p uId id' = withAccess (Proxy :: Proxy (ContextAPI a)) Proxy uId (PathNode $ contextId2NodeId id') contextAPI'
  where
    contextAPI' :: GargServer (ContextAPI a)
    contextAPI' = getContextWith   id' p
