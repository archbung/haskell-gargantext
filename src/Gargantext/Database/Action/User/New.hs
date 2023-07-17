{-|
Module      : Gargantext.Database.Action.User.New
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Database.Action.User.New
  (
    -- * Creating users
    newUser
  , newUsers
    -- * Helper functions
  , guessUserName
    -- * Internal types and functions for testing
  , new_user
  , mkNewUser
  )
  where

import Control.Lens (view)
import Control.Monad.Random
import Data.Text (Text, splitOn)
import Gargantext.Core.Mail
import Gargantext.Core.Mail.Types (HasMail, mailSettings)
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.Flow (getOrMkRoot)
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..), nodeError, NodeError(..))
import Gargantext.Database.Query.Table.User
import Gargantext.Prelude
import Gargantext.Prelude.Crypto.Pass.User (gargPass)
import Gargantext.Prelude.Mail.Types (MailConfig)
import qualified Data.Text as Text

------------------------------------------------------------------------
-- | Creates a new 'User' from the input 'EmailAddress', which needs to
-- be valid (i.e. a valid username needs to be inferred via 'guessUsername').
newUser :: (CmdM env err m, MonadRandom m, HasNodeError err, HasMail env)
        => EmailAddress
        -> m Int64
newUser emailAddress = do
  cfg <- view mailSettings
  pwd <- gargPass
  let nur = mkNewUser emailAddress (GargPassword pwd)
  affectedRows <- new_user nur
  withNotification (SendEmail True) cfg Invitation $ pure (affectedRows, nur)

------------------------------------------------------------------------
-- | A DB-specific action to create a single user.
-- This is an internal function and as such it /doesn't/ send out any email
-- notification, and thus lives in the 'DbCmd' effect stack. You may want to
-- use 'newUser' instead for standard Gargantext code.
new_user :: HasNodeError err
         => NewUser GargPassword
         -> DBCmd err Int64
new_user = new_users . (:[])

------------------------------------------------------------------------
-- | A DB-specific action to bulk-create users.
-- This is an internal function and as such it /doesn't/ send out any email
-- notification, and thus lives in the 'DbCmd' effect stack. You may want to
-- use 'newUsers' instead for standard Gargantext code.
new_users :: HasNodeError err
          => [NewUser GargPassword]
          -- ^ A list of users to create.
          -> DBCmd err Int64
new_users us = do
  us' <- liftBase         $ mapM toUserHash us
  r   <- insertUsers      $ map  toUserWrite us'
  _   <- mapM getOrMkRoot $ map  (\u -> UserName   (_nu_username u)) us
  pure r

------------------------------------------------------------------------
newUsers :: (CmdM env err m, MonadRandom m, HasNodeError err, HasMail env)
         => [EmailAddress]
         -> m Int64
newUsers us = do
  config <- view $ mailSettings
  us' <- mapM (\ea -> mkNewUser ea . GargPassword <$> gargPass) us
  newUsers' config us'

------------------------------------------------------------------------
mkNewUser :: EmailAddress -> GargPassword -> NewUser GargPassword
mkNewUser emailAddress pass =
  let  username = case guessUserName emailAddress of
        Just  (u', _m) -> u'
        Nothing        -> panic "[G.D.A.U.N.newUserQuick]: Email invalid"
  in (NewUser username (Text.toLower emailAddress) pass)

------------------------------------------------------------------------
-- | guessUserName
-- guess username and normalize it (Text.toLower)
guessUserName :: Text -> Maybe (Text,Text)
guessUserName n = case splitOn "@" n of
    [u',m'] -> if m' /= "" then Just (Text.toLower u',m')
                           else Nothing
    _       -> Nothing

------------------------------------------------------------------------
newUsers' :: HasNodeError err
         => MailConfig -> [NewUser GargPassword] -> Cmd err Int64
newUsers' cfg us = do
  us' <- liftBase         $ mapM toUserHash  us
  r   <- insertUsers      $ map  toUserWrite us'
  _   <- mapM getOrMkRoot $ map  (\u -> UserName   (_nu_username u)) us
  _   <- mapM (\u -> mail cfg (Invitation u)) us
  -- printDebug "newUsers'" us
  pure r

------------------------------------------------------------------------
-- | Updates a user's password, notifying the user via email, if necessary.
updateUser :: HasNodeError err
            => SendEmail -> MailConfig -> NewUser GargPassword -> Cmd err Int64
updateUser (SendEmail send) cfg u = do
  u' <- liftBase     $ toUserHash   u
  n  <- updateUserDB $ toUserWrite  u'
  when send $ mail cfg (PassUpdate u)
  pure n

------------------------------------------------------------------------
_updateUsersPassword :: (CmdM env err m, MonadRandom m, HasNodeError err, HasMail env)
         => [EmailAddress] -> m Int64
_updateUsersPassword us = do
  us' <- mapM (\ea -> mkNewUser ea . GargPassword <$> gargPass) us
  config <- view $ mailSettings
  _ <- mapM (\u -> updateUser (SendEmail True) config u) us'
  pure 1

------------------------------------------------------------------------
_rmUser :: HasNodeError err => User -> Cmd err Int64
_rmUser (UserName un) = deleteUsers [un]
_rmUser _ = nodeError NotImplYet

------------------------------------------------------------------------
-- TODO
_rmUsers :: HasNodeError err => [User] -> Cmd err Int64
_rmUsers [] = pure 0
_rmUsers _  = undefined
