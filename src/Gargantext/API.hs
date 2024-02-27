{-|
Module      : Gargantext.API
Description : REST API declaration
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main (RESTful) API of the instance Gargantext.

The Garg-API is typed to derive the documentation, the mock and tests.

This API is indeed typed in order to be able to derive both the server
and the client sides.

The Garg-API-Monad enables:
  - Security (WIP)
  - Features (WIP)
  - Database connection (long term)
  - In Memory stack management (short term)
  - Logs (WIP)

Thanks to Yann Esposito for our discussions at the start and to Nicolas
Pouillard (who mainly made it).

-}

{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
module Gargantext.API
      where

import Control.Concurrent
import Control.Lens hiding (Level)
import Data.List (lookup)
import Data.Text (pack)
import Data.Text.Encoding qualified as TE
import Data.Text.IO (putStrLn)
import Data.Validity
import Gargantext.API.Admin.Auth.Types (AuthContext)
import Gargantext.API.Admin.EnvTypes (Env, Mode(..))
import Gargantext.API.Admin.Settings (newEnv)
import Gargantext.API.Admin.Settings.CORS
import Gargantext.API.Admin.Types (FireWall(..), PortNumber, cookieSettings, jwtSettings, settings, corsSettings)
import Gargantext.API.EKG
import Gargantext.API.Middleware (logStdoutDevSanitised)
import Gargantext.API.Routes
import Gargantext.API.Server (server)
import Gargantext.Database.Prelude qualified as DB
import Gargantext.Prelude hiding (putStrLn)
import Gargantext.System.Logging
import Network.HTTP.Types hiding (Query)
import Network.Wai
import Network.Wai.Handler.Warp hiding (defaultSettings)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Paths_gargantext (getDataDir)
import Servant hiding (Header)
import System.Cron.Schedule qualified as Cron
import System.FilePath

-- | startGargantext takes as parameters port number and Ini file.
startGargantext :: Mode -> PortNumber -> FilePath -> IO ()
startGargantext mode port file = withLoggerHoisted mode $ \logger -> do
  env <- newEnv logger port file
  runDbCheck env
  portRouteInfo port
  app <- makeApp env
  mid <- makeGargMiddleware (env ^. settings.corsSettings) mode
  periodicActions <- schedulePeriodicActions env
  run port (mid app) `finally` stopGargantext periodicActions

  where runDbCheck env = do
          r <- runExceptT (runReaderT DB.dbCheck env) `catch`
            (\(_ :: SomeException) -> pure $ Right False)
          case r of
            Right True -> pure ()
            _ -> panicTrace $
              "You must run 'gargantext-init " <> pack file <>
              "' before running gargantext-server (only the first time)."

portRouteInfo :: PortNumber -> IO ()
portRouteInfo port = do
  putStrLn "      ----Main Routes-----      "
  putStrLn $ "http://localhost:" <> toUrlPiece port <> "/index.html"
  putStrLn $ "http://localhost:" <> toUrlPiece port <> "/swagger-ui"

-- | Stops the gargantext server and cancels all the periodic actions
-- scheduled to run up to that point.
-- TODO clean this Monad condition (more generic) ?
stopGargantext :: [ThreadId] -> IO ()
stopGargantext scheduledPeriodicActions = do
  forM_ scheduledPeriodicActions killThread
  putStrLn "----- Stopping gargantext -----"

-- | Schedules all sorts of useful periodic actions to be run while
-- the server is alive accepting requests.
schedulePeriodicActions :: DB.CmdCommon env => env -> IO [ThreadId]
schedulePeriodicActions _env =
  -- Add your scheduled actions here.
  let actions = [
          -- refreshDBViews
        ]
  in foldlM (\ !acc action -> (`mappend` acc) <$> Cron.execSchedule action) [] actions

{-
  where

    refreshDBViews :: Cron.Schedule ()
    refreshDBViews = do
      let doRefresh = do
            res <- DB.runCmd env (refreshNgramsMaterialized :: Cmd IOException ())
            case res of
              Left e   -> liftIO $ putStrLn $ pack ("Refreshing Ngrams materialized view failed: " <> displayException e)
              Right () ->  do
                _ <- liftIO $ putStrLn $ pack "Refresh Index Database done"
                pure ()
      Cron.addJob doRefresh "* 2 * * *"
-}

----------------------------------------------------------------------

fireWall :: Applicative f => Request -> FireWall -> f Bool
fireWall req fw = do
    let origin = lookup "Origin" (requestHeaders req)
    let host   = lookup "Host"   (requestHeaders req)

    if  origin == Just (encodeUtf8 "http://localhost:8008")
       && host == Just (encodeUtf8 "localhost:3000")
       || (not $ unFireWall fw)

       then pure True
       else pure False

makeGargMiddleware :: CORSSettings -> Mode -> IO Middleware
makeGargMiddleware crsSettings mode = do
    let corsMiddleware = cors $ \_incomingRq -> Just
          simpleCorsResourcePolicy
            { corsOrigins = Just (map mkCorsOrigin (crsSettings ^. corsAllowedOrigins), True)
            , corsMethods = [ methodGet   , methodPost   , methodPut
                            , methodDelete, methodOptions, methodHead]
            , corsIgnoreFailures = False
            , corsRequestHeaders = ["authorization", "content-type", "x-garg-error-scheme"]
            , corsMaxAge         = Just ( 60*60*24 ) -- one day
            }
    case mode of
      Prod -> pure $ logStdout . corsMiddleware
      _    -> do
        loggerMiddleware <- logStdoutDevSanitised
        pure $ loggerMiddleware . corsMiddleware
  where
    mkCorsOrigin :: CORSOrigin -> Origin
    mkCorsOrigin = TE.encodeUtf8 . _CORSOrigin

---------------------------------------------------------------------
-- | API Global
---------------------------------------------------------------------

makeApp :: Env -> IO Application
makeApp env = do
  serv <- server env
  (ekgStore, ekgMid) <- newEkgStore api
  ekgDir <- (</> "ekg-assets") <$> getDataDir
  pure $ ekgMid $ serveWithContext apiWithEkg cfg
    (ekgServer ekgDir ekgStore :<|> serv)
  where
    cfg :: Servant.Context AuthContext
    cfg = env ^. settings . jwtSettings
       :. env ^. settings . cookieSettings
       :. EmptyContext

---------------------------------------------------------------------
api :: Proxy API
api  = Proxy

apiWithEkg :: Proxy (EkgAPI :<|> API)
apiWithEkg = Proxy

apiGarg :: Proxy GargAPI
apiGarg  = Proxy
---------------------------------------------------------------------
