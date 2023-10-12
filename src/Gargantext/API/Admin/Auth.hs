{-|
Module      : Gargantext.API.Admin.Auth
Description : Server API Auth Module
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main authorization of Gargantext are managed in this module

-- 1: Implement the Server / Client JWT authentication
      -> Client towards Python Backend
      -> Server towards Purescript Front-End

-- 2: Implement the Auth API backend
    https://github.com/haskell-servant/servant-auth

TODO-ACCESS Critical

To see the authors:
- gource src
And you have the main viz

-}

{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators      #-}

module Gargantext.API.Admin.Auth
  ( auth
  , withPolicy
  , withPolicyT
  , forgotPassword
  , forgotPasswordAsync
  , withAccess
  , ForgotPasswordAPI
  , ForgotPasswordAsyncParams
  , ForgotPasswordAsyncAPI
  )
  where

--import Control.Monad.Logger.Aeson
--import qualified Text.Blaze.Html5.Attributes as HA
import Control.Lens (view, (#))
import Data.Aeson
import Data.Swagger (ToSchema(..))
import Data.UUID (UUID, fromText, toText)
import Data.UUID.V4 (nextRandom)
import Gargantext.API.Admin.Auth.Types
import Gargantext.API.Admin.EnvTypes (GargJob(..), Env)
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Admin.Types
import Gargantext.API.Prelude (HasJoseError(..), joseError, HasServerError, GargServerC, GargServer, _ServerError, GargM, GargError (..))
import Gargantext.Core.Mail (MailModel(..), mail)
import Gargantext.Core.Mail.Types (mailSettings)
import Gargantext.Core.Types.Individu (User(..), Username, GargPassword(..))
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Types.Node (NodeId(..))
import Gargantext.Database.Prelude (Cmd', CmdCommon, DbCmd')
import Gargantext.Database.Query.Table.User
import Gargantext.Database.Query.Tree (isDescendantOf, isIn)
import Gargantext.Database.Query.Tree.Root (getRoot)
import Gargantext.Database.Action.User.New (guessUserName)
import Gargantext.Database.Schema.Node (NodePoly(_node_id))
import Gargantext.Prelude hiding (reverse)
import Gargantext.Prelude.Crypto.Pass.User (gargPass)
import Gargantext.Utils.Jobs (serveJobsAPI, MonadJobStatus(..))
import Protolude hiding (Handler, to)
import Servant
import Servant.Auth.Server
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as LE
import qualified Gargantext.Prelude.Crypto.Auth as Auth
import Gargantext.API.Auth.PolicyCheck

---------------------------------------------------

-- | Main functions of authorization

makeTokenForUser :: ( HasSettings env
                    , HasJoseError err )
                 => NodeId -> Cmd' env err Token
makeTokenForUser uid = do
  jwtS <- view $ settings . jwtSettings
  e <- liftBase $ makeJWT (AuthenticatedUser uid) jwtS Nothing
  -- TODO-SECURITY here we can implement token expiration ^^.
  either joseError (pure . toStrict . LE.decodeUtf8) e
  -- TODO not sure about the encoding...

checkAuthRequest :: ( HasSettings env, HasJoseError err, DbCmd' env err m )
                 => Username
                 -> GargPassword
                 -> m CheckAuth
checkAuthRequest couldBeEmail (GargPassword p) = do
  -- Sometimes user put email instead of username
  -- hence we have to check before
  let usrname = case guessUserName couldBeEmail of
        Nothing      -> couldBeEmail -- we are sure this is not an email
        Just (u,_)   -> u            -- this was an email in fact

  candidate <- head <$> getUsersWith usrname
  case candidate of
    Nothing -> pure InvalidUser
    Just (UserLight { userLight_password = GargPassword h, .. }) ->
      case Auth.checkPassword (Auth.mkPassword p) (Auth.PasswordHash h) of
        Auth.PasswordCheckFail    -> pure InvalidPassword
        Auth.PasswordCheckSuccess -> do
          muId <- head <$> getRoot (UserName usrname)
          case _node_id <$> muId of
            Nothing  -> pure InvalidUser
            Just uid -> do
              token <- makeTokenForUser uid
              pure $ Valid token uid userLight_id

auth :: (HasSettings env, HasJoseError err, DbCmd' env err m)
     => AuthRequest -> m AuthResponse
auth (AuthRequest u p) = do
  checkAuthRequest' <- checkAuthRequest u p
  case checkAuthRequest' of
    InvalidUser     -> pure $ AuthResponse Nothing (Just $ AuthInvalid "Invalid username or password")
    InvalidPassword -> pure $ AuthResponse Nothing (Just $ AuthInvalid "Invalid username or password")
    Valid to trId uId   -> pure $ AuthResponse (Just $ AuthValid to trId uId) Nothing

--type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

{-
instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

authCheck :: forall env. env
          -> BasicAuthData
          -> IO (AuthResult AuthenticatedUser)
authCheck _env (BasicAuthData login password) = pure $
  maybe Indefinite Authenticated $ TODO
-}

withAccessM :: ( DbCmd' env err m )
            => AuthenticatedUser
            -> PathId
            -> m a
            -> m a
withAccessM (AuthenticatedUser uId) (PathNode id) m = do
  d <- id `isDescendantOf` uId
  if d then m else m -- serverError err401
withAccessM (AuthenticatedUser uId) (PathNodeNode cId docId) m = do
  _a <- isIn cId docId -- TODO use one query for all ?
  _d <- cId `isDescendantOf` uId
  if True -- a && d
     then m
     else m -- serverError err401

withAccess :: forall env err m api.
              (GargServerC env err m, HasServer api '[]) =>
              Proxy api -> Proxy m -> AuthenticatedUser -> PathId ->
              ServerT api m -> ServerT api m
withAccess p _ ur id = hoistServer p f
  where
    f :: forall a. m a -> m a
    f = withAccessM ur id

-- | Given the 'AuthenticatedUser', a policy check and a function that returns an @a@,
-- it runs the underlying policy check to ensure that the resource is returned only to
-- who is entitled to see it.
withPolicy :: GargServerC env GargError m
           => AuthenticatedUser
           -> BoolExpr AccessCheck
           -> m a
           -> AccessPolicyManager
           -> m a
withPolicy ur checks m mgr = case mgr of
  AccessPolicyManager{runAccessPolicy} -> do
    res <- runAccessPolicy ur checks
    case res of
      Allow     -> m
      Deny err  -> throwError $ GargServerError $ err

withPolicyT :: forall env m api. (
                 GargServerC env GargError m
               , HasServer api '[]
               )
            => Proxy api
            -> Proxy m
            -> AuthenticatedUser
            -> BoolExpr AccessCheck
            -> ServerT api m
            -> AccessPolicyManager
            -> ServerT api m
withPolicyT p _ ur checks m0 mgr = hoistServer p f m0
  where
    f :: forall a. m a -> m a
    f m = withPolicy ur checks m mgr

{- | Collaborative Schema
User at his root can create Teams Folder
User can create Team in Teams Folder.
User can invite User in Team as NodeNode only if Team in his parents.
All users can access to the Team folder as if they were owner.
-}

newtype ForgotPasswordAsyncParams =
  ForgotPasswordAsyncParams { email :: Text }
  deriving (Generic, Show)
instance FromJSON ForgotPasswordAsyncParams where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON ForgotPasswordAsyncParams where
  toJSON = genericToJSON defaultOptions
instance ToSchema ForgotPasswordAsyncParams

type ForgotPasswordAPI = Summary "Forgot password POST API"
                           :> ReqBody '[JSON] ForgotPasswordRequest
                           :> Post '[JSON] ForgotPasswordResponse
                         :<|> Summary "Forgot password GET API"
                           :> QueryParam "uuid" Text
                           :> Get '[JSON] ForgotPasswordGet


forgotPassword :: GargServer ForgotPasswordAPI
     -- => ForgotPasswordRequest -> Cmd' env err ForgotPasswordResponse
forgotPassword = forgotPasswordPost :<|> forgotPasswordGet

forgotPasswordPost :: (CmdCommon env)
     => ForgotPasswordRequest -> Cmd' env err ForgotPasswordResponse
forgotPasswordPost (ForgotPasswordRequest email) = do
  us <- getUsersWithEmail (Text.toLower email)
  case us of
    [u] -> forgotUserPassword u
    _ -> pure ()

  -- NOTE Sending anything else here could leak information about
  -- users' emails
  pure $ ForgotPasswordResponse "ok"

forgotPasswordGet :: (HasSettings env, CmdCommon env, HasJoseError err, HasServerError err)
     => Maybe Text -> Cmd' env err ForgotPasswordGet
forgotPasswordGet Nothing = pure $ ForgotPasswordGet ""
forgotPasswordGet (Just uuid) = do
  let mUuid = fromText uuid
  case mUuid of
    Nothing -> throwError $ _ServerError # err404 { errBody = "Not found" }
    Just uuid' -> do
      -- fetch user
      us <- getUsersWithForgotPasswordUUID uuid'
      case us of
        [u] -> forgotPasswordGetUser u
        _ -> throwError $ _ServerError # err404 { errBody = "Not found" }

---------------------

forgotPasswordGetUser :: ( HasSettings env, CmdCommon env, HasJoseError err, HasServerError err)
     => UserLight -> Cmd' env err ForgotPasswordGet
forgotPasswordGetUser (UserLight { .. }) = do
  -- pick some random password
  password <- liftBase gargPass

  -- set it as user's password
  hashed <- liftBase $ Auth.hashPassword $ Auth.mkPassword password
  let hashed' = Auth.unPasswordHash hashed
  let userPassword = UserLight { userLight_password = GargPassword hashed', .. }
  _ <- updateUserPassword userPassword

  -- display this briefly in the html

  -- clear the uuid so that the page can't be refreshed
  _ <- updateUserForgotPasswordUUID $ UserLight { userLight_forgot_password_uuid = Nothing, .. }

  pure $ ForgotPasswordGet password

forgotUserPassword :: (CmdCommon env)
     => UserLight -> Cmd' env err ()
forgotUserPassword (UserLight { .. }) = do
  --printDebug "[forgotUserPassword] userLight_id" userLight_id
  --logDebug $ "[forgotUserPassword]" :# ["userLight_id" .= userLight_id]
  -- generate uuid for email
  uuid <- generateForgotPasswordUUID

  let userUUID = UserLight { userLight_forgot_password_uuid = Just $ toText uuid, .. }

  -- save user with that uuid
  _ <- updateUserForgotPasswordUUID userUUID

  -- send email with uuid link
  cfg <- view $ mailSettings
  mail cfg (ForgotPassword { user = userUUID })

  -- on uuid link enter: change user password and present it to the
  -- user

  pure ()

--------------------------

-- Generate a unique (in whole DB) UUID for passwords.
generateForgotPasswordUUID :: (CmdCommon env)
  => Cmd' env err UUID
generateForgotPasswordUUID = do
  uuid <- liftBase $ nextRandom
  us <- getUsersWithForgotPasswordUUID uuid
  case us of
    [] -> pure uuid
    _ -> generateForgotPasswordUUID

----------------------------

-- NOTE THe async endpoint is better for the "forget password"
-- request, because the delay in email sending etc won't reveal to
-- malicious users emails of our users in the db
type ForgotPasswordAsyncAPI = Summary "Forgot password asnc"
                              :> AsyncJobs JobLog '[JSON] ForgotPasswordAsyncParams JobLog

forgotPasswordAsync :: ServerT ForgotPasswordAsyncAPI (GargM Env GargError)
forgotPasswordAsync =
  serveJobsAPI ForgotPasswordJob $ \jHandle p -> forgotPasswordAsync' p jHandle

forgotPasswordAsync' :: (FlowCmdM env err m, MonadJobStatus m)
  => ForgotPasswordAsyncParams
  -> JobHandle m
  -> m ()
forgotPasswordAsync' (ForgotPasswordAsyncParams { email }) jobHandle = do

  markStarted 2 jobHandle
  markProgress 1 jobHandle

  -- printDebug "[forgotPasswordAsync'] email" email

  _ <- forgotPasswordPost $ ForgotPasswordRequest { _fpReq_email = email }

  markComplete jobHandle
