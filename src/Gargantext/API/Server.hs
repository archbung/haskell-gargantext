{-|
Module      : Gargantext.API.Server
Description : REST API declaration
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Gargantext.API.Server where

import Control.Lens ((^.))
import Control.Monad.Catch (catch, throwM)
import Data.Text qualified as T
import Data.Version (showVersion)
import Gargantext.API.Admin.Auth (auth, forgotPassword, forgotPasswordAsync)
import Gargantext.API.Admin.Auth.Types (AuthContext)
import Gargantext.API.Admin.EnvTypes (Env)
import Gargantext.API.Admin.FrontEnd (frontEndServer)
import Gargantext.API.Auth.PolicyCheck ()
import Gargantext.API.Errors
import Gargantext.API.GraphQL qualified as GraphQL
import Gargantext.API.Prelude
import Gargantext.API.Public qualified as Public
import Gargantext.API.Routes
import Gargantext.API.Swagger (swaggerDoc)
import Gargantext.API.ThrowAll (serverPrivateGargAPI)
import Gargantext.Database.Prelude (hasConfig)
import Gargantext.Prelude hiding (Handler, catch)
import Gargantext.Prelude.Config (gc_url_backend_api)
import Gargantext.System.Logging
import Paths_gargantext qualified as PG -- cabal magic build module
import Servant
import Servant.Swagger.UI (swaggerSchemaUIServer)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE


serverGargAPI :: Text -> ServerT GargAPI (GargM Env BackendInternalError)
serverGargAPI baseUrl -- orchestrator
       =  auth
     :<|> forgotPassword
     :<|> forgotPasswordAsync
     :<|> gargVersion
     :<|> serverPrivateGargAPI
     :<|> Public.api baseUrl

  --   :<|> orchestrator
  where
    gargVersion :: GargServer GargVersion
    gargVersion = pure (cs $ showVersion PG.version)

-- | Server declarations
server :: Env -> IO (Server API)
server env = do
  -- orchestrator <- scrapyOrchestrator env
  pure $ \errScheme -> swaggerSchemaUIServer swaggerDoc
     :<|> hoistServerWithContext
            (Proxy :: Proxy GargAPI)
            (Proxy :: Proxy AuthContext)
            (transformJSON errScheme)
            (serverGargAPI (env ^. hasConfig . gc_url_backend_api))
     :<|> hoistServerWithContext
            (Proxy :: Proxy GraphQL.API)
            (Proxy :: Proxy AuthContext)
            (transformJSONGQL errScheme)
            GraphQL.api
     :<|> frontEndServer
  where
    transformJSON :: forall a. GargErrorScheme -> GargM Env BackendInternalError a -> Handler a
    transformJSON GES_old = Handler . withExceptT showAsServantJSONErr . (`runReaderT` env) . logPanicErrors
    transformJSON GES_new = Handler . withExceptT (frontendErrorToServerError . backendErrorToFrontendError) . (`runReaderT` env) . handlePanicErrors
    transformJSONGQL :: forall a. GargErrorScheme -> GargM Env BackendInternalError a -> Handler a
    transformJSONGQL GES_old = Handler . withExceptT showAsServantJSONErr . (`runReaderT` env) . logPanicErrors
    transformJSONGQL GES_new = Handler . withExceptT (frontendErrorToGQLServerError . backendErrorToFrontendError) . (`runReaderT` env) . handlePanicErrors

handlePanicErrors :: GargM Env BackendInternalError a -> GargM Env BackendInternalError a
handlePanicErrors h = h `catch` handleSomeException
  where
    handleSomeException :: SomeException -> GargM Env BackendInternalError a
    handleSomeException se
      | Just ex@(WithStacktrace _ (_ :: UnexpectedPanic)) <- fromException se
      = do
        $(logLocM) ERROR $ T.pack $ displayException ex
        ReaderT $ \_ -> ExceptT $ pure $ Left $ InternalUnexpectedError se
      | Just (ber :: BackendInternalError) <- fromException se
      = throwError ber -- re-throw the uncaught exception via the 'MonadError' instance
      | otherwise
      = throwM se -- re-throw the uncaught exception.

-- | Old compat-shim for the old error format, it just logs the exception properly
-- but otherwise rethrows it /without/ the stacktrace (to not leak internal details).
logPanicErrors :: GargM Env BackendInternalError a -> GargM Env BackendInternalError a
logPanicErrors h = h `catch` handleSomeException
  where
    handleSomeException :: SomeException -> GargM Env BackendInternalError a
    handleSomeException se
      | Just ex@(WithStacktrace _ (UnexpectedPanic uex)) <- fromException se
      = do
        $(logLocM) ERROR $ T.pack $ displayException ex
        throwError $ InternalServerError $ err500 { errBody = BL.fromStrict $ TE.encodeUtf8 uex }
      | Just (ber :: BackendInternalError) <- fromException se
      = throwError ber -- re-throw the uncaught exception via the 'MonadError' instance
      | otherwise
      = throwM se -- re-throw the uncaught exception.

