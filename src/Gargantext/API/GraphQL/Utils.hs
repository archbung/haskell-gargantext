{-|
Module      : Gargantext.API.GraphQL.Utils
Description : Utils for GraphQL API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -Wno-deprecations #-} -- FIXME(adn) GraphQL will need updating.

module Gargantext.API.GraphQL.Utils where

import Control.Lens.Getter (view)
import Data.Morpheus.Types (GQLTypeOptions, fieldLabelModifier)
import Data.Text qualified as T
import Gargantext.API.Admin.Auth.Types (AuthenticatedUser (..), auth_node_id)
import Gargantext.API.Admin.Types (jwtSettings, HasSettings (settings))
import Gargantext.Core.Utils.Prefix (unCapitalize, dropPrefix)
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Database.Prelude (Cmd')
import Gargantext.Prelude
import Servant.Auth.Server (verifyJWT, JWTSettings)
import Control.Lens ((^.))

unPrefix :: T.Text -> GQLTypeOptions -> GQLTypeOptions
unPrefix prefix options = options { fieldLabelModifier = nflm }
  where
    nflm label = unCapitalize $ dropPrefix (T.unpack prefix) $ ( fieldLabelModifier options ) label

data AuthStatus = Valid | Invalid

authUser :: (HasSettings env) => NodeId -> Text -> Cmd' env err AuthStatus
authUser ui_id token = do
  let token' = encodeUtf8 token
  jwtS <- view $ settings . jwtSettings
  u <- liftBase $ getUserFromToken jwtS token'
  case u of
    Nothing -> pure Invalid
    Just au ->
      if au ^. auth_node_id == ui_id
        then pure Valid
        else pure Invalid

getUserFromToken :: JWTSettings -> ByteString -> IO (Maybe AuthenticatedUser)
getUserFromToken = verifyJWT
