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
import Test.API.Setup (withTestDBAndPort, setupEnvironment, createAliceAndBob)
import Test.Hspec
import Test.Hspec.Wai.Internal (withApplication)
import Test.Hspec.Wai.JSON (json)
import Test.Utils (protected, protectedNewError, shouldRespondWithFragment, shouldRespondWithFragmentCustomStatus, withValidLogin)
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
            let query = [r| { "query": "{ user_infos(user_id: 2) { ui_id, ui_email } }" } |]
            let expected = [json| {"data":{"user_infos":[{"ui_id":2,"ui_email":"alice@gargan.text"}]}} |]
            protected token "POST" "/gql" query `shouldRespondWithFragment` expected


    describe "check error format" $ do
      it "returns the new error if header X-Garg-Error-Scheme: new is passed" $ \((_testEnv, port), app) -> do
        withApplication app $ do
          withValidLogin port "gargantua" (GargPassword "secret_key") $ \token -> do
            let query = [r| { "query": "{ languages(id:5) { lt_lang } }" } |]
            let expected = [json| {"errors": [{"locations":[{"column":13,"line":1}],"message":"Unknown Argument \"id\" on Field \"languages\"."}] } |]
            protectedNewError token "POST" "/gql" query `shouldRespondWithFragment` expected

      it "returns the old error (though this is deprecated)" $ \((_testEnv, port), app) -> do
        withApplication app $ do
          withValidLogin port "gargantua" (GargPassword "secret_key") $ \token -> do
            let query = [r| { "query": "{ languages(id:5) { lt_lang } }" } |]
            let expected = [json| {"errors": [{"locations":[{"column":13,"line":1}],"message":"Unknown Argument \"id\" on Field \"languages\"."}] } |]
            protected token "POST" "/gql" query `shouldRespondWithFragment` expected

      it "check new errors with 'type'" $ \((_testEnv, port), app) -> do
        withApplication app $ do
          withValidLogin port "gargantua" (GargPassword "secret_key") $ \token -> do
            let query = [r| { "query": "mutation { delete_team_membership(shared_folder_id:1, team_node_id:1, token:\"abc\") }" } |]
            let expected = [json| {"errors":[{"extensions":{"data":{"msg":"This user is not team owner","user_id":1},"diagnostic":"User not authorized. ","type":"EC_403__user_not_authorized"},"message":"User not authorized. "}]} |]
            shouldRespondWithFragmentCustomStatus 403
              (protectedNewError token "POST" "/gql" query)
              expected
