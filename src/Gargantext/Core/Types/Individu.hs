{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Gargantext.Core.Types.Individu
Description : Short description
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Individu defintions
-}



module Gargantext.Core.Types.Individu
  where

import Data.Aeson
import Data.Swagger
import Data.Text (pack, reverse)
import Data.Text qualified as T
import Gargantext.Database.Admin.Types.Node (NodeId, UserId)
import Gargantext.Prelude hiding (reverse)
import Gargantext.Prelude.Crypto.Auth qualified as Auth
import Prelude qualified

-- FIXME UserName used twice
data User = UserDBId UserId | UserName Text | RootId NodeId | UserPublic
  deriving (Eq)

renderUser :: User -> T.Text
renderUser = \case
  UserDBId urId -> T.pack (show urId)
  UserName txt  -> txt
  RootId   nId  -> T.pack (show nId)
  UserPublic    -> T.pack "public"

type Username = Text

type HashPassword    = Auth.PasswordHash Auth.Argon2
newtype GargPassword = GargPassword Text
  deriving (Generic)

toGargPassword :: Text -> GargPassword
toGargPassword x = GargPassword x

instance Prelude.Show GargPassword where
  show (GargPassword _) = "*GargPassword*"

instance ToJSON GargPassword
instance FromJSON GargPassword

instance ToSchema GargPassword
type Email          = Text
type UsernameMaster = Username
type UsernameSimple = Username

data NewUser a = NewUser { _nu_username :: Username
                         , _nu_email    :: Email
                         , _nu_password :: a
                         }
  deriving (Show)

arbitraryUsername :: [Username]
arbitraryUsername = {- ["gargantua"] <> -} users
  where
    users = zipWith (\a b -> a <> (pack . show) b) 
                    (repeat "user") ([1..20]::[Int])

arbitraryPassword :: [GargPassword]
arbitraryPassword = map (\u -> GargPassword (reverse u)) arbitraryUsername

-----------------------------------------------------------
toUserHash :: MonadIO m
         =>    NewUser GargPassword
         -> m (NewUser HashPassword)
toUserHash (NewUser u m (GargPassword p)) = do
  salt <- Auth.newSalt
  let h = Auth.hashPasswordWithSalt params salt (Auth.mkPassword p)
  pure $ NewUser u m h
  where
#if TEST_CRYPTO
    params = Auth.defaultParams { Auth.argon2MemoryCost = 4096 }
#else
    params = Auth.defaultParams
#endif

-- TODO remove
arbitraryUsersHash :: MonadIO m
                  => m [NewUser HashPassword]
arbitraryUsersHash = mapM toUserHash arbitraryNewUsers

arbitraryNewUsers :: [NewUser GargPassword]
arbitraryNewUsers = map (\u -> NewUser u (u <> "@gargantext.org") (GargPassword $ reverse u))
                     arbitraryUsername


