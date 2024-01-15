{-|
Module      : Gargantext.API.Admin.Auth.Types
Description : Types for Server API Auth Module
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell #-}

module Gargantext.API.Admin.Auth.Types
      where

import Control.Lens hiding (elements, to)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.TH as JSON
import Data.List (tail)
import Data.Swagger
import Gargantext.Core.Types.Individu (Username, GargPassword(..), arbitraryUsername, arbitraryPassword)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Admin.Types.Node (NodeId(..), ListId, DocId, UserId (..))
import Gargantext.Prelude hiding (reverse)
import Servant.Auth.Server
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Crypto.JWT as Jose

---------------------------------------------------

-- | Main types for AUTH API
data AuthRequest = AuthRequest { _authReq_username :: Username
                               , _authReq_password :: GargPassword
                               }
  deriving (Generic)

data AuthResponse = AuthResponse { _authRes_token   :: Token
                                 , _authRes_tree_id :: TreeId
                                 , _authRes_user_id :: UserId
                                 }
  deriving (Generic, Eq, Show)

type Token  = Text
type TreeId = NodeId

data CheckAuth = InvalidUser | InvalidPassword | Valid Token TreeId UserId
  deriving (Eq)

data AuthenticatedUser = AuthenticatedUser
  { _auth_node_id :: NodeId
  , _auth_user_id :: UserId
  } deriving (Generic)

$(deriveJSON (JSON.defaultOptions { JSON.fieldLabelModifier = tail . dropWhile ((/=) '_') . tail }) ''AuthenticatedUser)

makeLenses ''AuthenticatedUser

instance ToSchema AuthenticatedUser where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_authUser_")

instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

data AuthenticationError
  = LoginFailed NodeId UserId Jose.Error
  | InvalidUsernameOrPassword
  deriving (Show, Eq)

-- TODO-SECURITY why is the CookieSettings necessary?
type AuthContext = '[JWTSettings, CookieSettings] -- , BasicAuthCfg

-- | Instances
$(deriveJSON (unPrefix "_authReq_") ''AuthRequest)
instance ToSchema AuthRequest where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_authReq_")

instance Arbitrary AuthRequest where
  arbitrary = elements [ AuthRequest u p
                       | u <- arbitraryUsername
                       , p <- arbitraryPassword
                       ]

$(deriveJSON (unPrefix "_authRes_") ''AuthResponse)
instance ToSchema AuthResponse where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_authRes_")
instance Arbitrary AuthResponse where
  arbitrary = elements [ AuthResponse to' tr u
                       | to' <- ["token0", "token1"]
                       , tr <- map UnsafeMkNodeId [1..3]
                       , u <-  map UnsafeMkUserId [1..3]
                       ]

data PathId = PathNode NodeId | PathNodeNode ListId DocId


---------------------------

type Email = Text
type Password = Text

data ForgotPasswordRequest = ForgotPasswordRequest { _fpReq_email :: Email }
  deriving (Generic )
$(deriveJSON (unPrefix "_fpReq_") ''ForgotPasswordRequest)
instance ToSchema ForgotPasswordRequest where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_fpReq_")

data ForgotPasswordResponse = ForgotPasswordResponse { _fpRes_status :: Text }
  deriving (Generic )
$(deriveJSON (unPrefix "_fpRes_") ''ForgotPasswordResponse)
instance ToSchema ForgotPasswordResponse where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_fpRes_")

data ForgotPasswordGet = ForgotPasswordGet {_fpGet_password :: Password}
  deriving (Generic )
$(deriveJSON (unPrefix "_fpGet_") ''ForgotPasswordGet)
instance ToSchema ForgotPasswordGet where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_fpGet_")

makeLenses ''AuthResponse
