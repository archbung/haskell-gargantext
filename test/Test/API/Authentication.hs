{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.API.Authentication where

import Prelude
import Control.Concurrent.MVar
import Data.Proxy
import Gargantext.API (makeApp)
import Gargantext.API.Admin.EnvTypes (Mode(Mock))
import Gargantext.API.Admin.Settings (newEnv)
import Gargantext.API.Routes
import Gargantext.System.Logging
import Network.HTTP.Client hiding (Proxy)
import Servant.Client
import Test.Database.Setup (withTestDB, fakeIniPath)
import Test.Hspec
import qualified Network.Wai.Handler.Warp         as Warp
import Test.Database.Types

withGargApp :: (Warp.Port -> IO ()) -> IO ()
withGargApp action = do
  randomPort <- newEmptyMVar
  let createApp = do
        port <- readMVar randomPort
        withLoggerHoisted Mock $ \ioLogger -> do
          ini <- fakeIniPath
          env <- newEnv ioLogger port ini
          makeApp env
  Warp.testWithApplication createApp (\p -> putMVar randomPort p >> action p)

withTestDBAndPort :: ((TestEnv, Warp.Port) -> IO ()) -> IO ()
withTestDBAndPort action =
  withTestDB $ \testEnv ->
    withGargApp $ \port ->
      action (testEnv, port)

tests :: Spec
tests = sequential $ aroundAll withTestDBAndPort $ do
  describe "Authentication" $ do
    let getVersion = client (Proxy :: Proxy GargVersion)
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    -- testing scenarios start here
    describe "GET /version" $ do
      it "requires no auth" $ \(_testEnv, port) -> do
        result <- runClientM getVersion (clientEnv port)
        result `shouldBe` (Right "foo")
