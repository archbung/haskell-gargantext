{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

module Test.API.GraphQL (
    tests
  ) where

import Gargantext.Core.Types.Individu
import Prelude
import Servant.Auth.Client ()
import Test.API.Private (withValidLogin, protected)
import Test.API.Setup (withTestDBAndPort, setupEnvironment, createAliceAndBob)
import Test.Hspec
import Test.Hspec.Wai.Internal (withApplication)
import Test.Utils
import Text.RawString.QQ (r)

tests :: Spec
tests = sequential $ aroundAll withTestDBAndPort $ do
  describe "Prelude" $ do
    it "setup DB triggers" $ \((testEnv, _), _) -> setupEnvironment testEnv
  describe "GraphQL" $ do


    describe "get_user_infos" $ do
      it "allows 'alice' to see her own info" $ \((testEnv, port), app) -> do
        createAliceAndBob testEnv

        withApplication app $ do
          withValidLogin port "alice" (GargPassword "alice") $ \token -> do
            protected token "POST" "/gql" [r| {
               "query": "{ user_infos(user_id: 2) { ui_id, ui_email } }"
               } |] `shouldRespondWith'` [jsonFragment| {"data":{"user_infos":[{"ui_id":2,"ui_email":"alice@gargan.text"}]}} |]
