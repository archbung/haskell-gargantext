{-|
Module      : Gargantext.API.HashedResponse
Description :
Copyright   : (c) CNRS, 2020-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.API.HashedResponse where

import Data.Aeson
import Data.Swagger
import Gargantext.Prelude
import Gargantext.Prelude.Crypto.Hash qualified as Crypto (hash)

data HashedResponse a = HashedResponse { hash :: Text, value :: a }
  deriving (Generic)

instance ToSchema a => ToSchema (HashedResponse a)
instance ToJSON a => ToJSON (HashedResponse a) where
  toJSON = genericToJSON defaultOptions
instance FromJSON a => FromJSON (HashedResponse a) where
  parseJSON = genericParseJSON defaultOptions

constructHashedResponse :: ToJSON a => a -> HashedResponse a
constructHashedResponse v = HashedResponse { hash = Crypto.hash $ encode v, value = v }
