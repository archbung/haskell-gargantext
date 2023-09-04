{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Database.Operations.DocumentSearch where

import Prelude

import Control.Monad.Reader
import Data.Aeson.QQ.Simple
import Data.Aeson.Types
import Data.Maybe
import Gargantext.Core
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.Flow
import Gargantext.Database.Action.Search
import Gargantext.Database.Admin.Types.Hyperdata.Document
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Tree.Root
import Gargantext.Database.Schema.Node (NodePoly(..))
import Network.URI (parseURI)

import Database.Operations.Types
import Test.Hspec.Expectations
import Test.Tasty.HUnit
import Gargantext.Core.Text.Terms.Mono.Stem.En
import Gargantext.Database.Admin.Config (userMaster)


exampleDocument_01 :: HyperdataDocument
exampleDocument_01 = either error id $ parseEither parseJSON $ [aesonQQ|
  { "doi":"sdfds"
  , "publication_day":6
  , "language_iso2":"EN"
  , "publication_minute":0
  , "publication_month":7
  , "language_iso3":"eng"
  , "publication_second":0
  , "authors":"Nils Hovdenak, Kjell Haram"
  , "publication_year":2012
  , "publication_date":"2012-07-06 00:00:00+00:00"
  , "language_name":"English"
  , "realdate_full_":"2012 01 12"
  , "source":"European journal of obstetrics, gynecology, and reproductive biology Institute"
  , "abstract":"The literature was searched for publications on minerals and vitamins during pregnancy and the possible influence of supplements on pregnancy outcome."
  , "title":"Influence of mineral and vitamin supplements on pregnancy outcome."
  , "publication_hour":0
  }
|]

exampleDocument_02 :: HyperdataDocument
exampleDocument_02 = either error id $ parseEither parseJSON $ [aesonQQ|
  { "doi":"sdfds"
  , "publication_day":6
  , "language_iso2":"EN"
  , "publication_minute":0
  , "publication_month":7
  , "language_iso3":"eng"
  , "publication_second":0
  , "authors":"Ajeje Brazorf and Manuel Agnelli"
  , "publication_year":2012
  , "publication_date":"2012-07-06 00:00:00+00:00"
  , "language_name":"English"
  , "realdate_full_":"2012 01 12"
  , "source":"Malagrotta Institute of Technology"
  , "abstract":"We present PyPlasm, an innovative approach to computational graphics"
  , "title":"PyPlasm: computational geometry made easy"
  , "publication_hour":0
  }
|]

nlpServerConfig :: NLPServerConfig
nlpServerConfig =
  let uri = parseURI "http://localhost:9000"
  in NLPServerConfig CoreNLP (fromMaybe (error "parseURI for nlpServerConfig failed") uri)

corpusAddDocuments :: TestEnv -> Assertion
corpusAddDocuments env = do
  flip runReaderT env $ runTestMonad $ do
    -- NOTE(adn) We need to create user 'gargantua'(!!) in order
    -- for 'addDocumentsToHyperCorpus' to work.
    parentId <- getRootId (UserName userMaster)
    [corpus] <- getCorporaWithParentId parentId
    let corpusId = _node_id corpus

    ids <- addDocumentsToHyperCorpus nlpServerConfig
                                     (Just $ _node_hyperdata $ corpus)
                                     (Multi EN)
                                     corpusId
                                     [exampleDocument_01, exampleDocument_02]
    liftIO $ length ids `shouldBe` 2

stemmingTest :: TestEnv -> Assertion
stemmingTest _env = do
  stemIt "Ajeje"    `shouldBe` "Ajeje"
  stemIt "PyPlasm:" `shouldBe` "PyPlasm:"

corpusSearch01 :: TestEnv -> Assertion
corpusSearch01 env = do
  flip runReaderT env $ runTestMonad $ do

    parentId <- getRootId (UserName "gargantua")
    [corpus] <- getCorporaWithParentId parentId

    results1 <- searchInCorpus (_node_id corpus) False ["mineral"] Nothing Nothing Nothing
    results2 <- searchInCorpus (_node_id corpus) False ["computational"] Nothing Nothing Nothing

    liftIO $ length results1 `shouldBe` 1
    liftIO $ length results2 `shouldBe` 1
