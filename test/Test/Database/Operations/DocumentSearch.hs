{-|
Module      : Test.Database.Operations.DocumentSearch
Description : GarganText database tests
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Database.Operations.DocumentSearch where

import Prelude

import Control.Lens (view)
import Control.Monad.Reader
import Data.Aeson.QQ.Simple
import Data.Aeson.Types
-- import Gargantext.API.Node.Update (updateDocs)
import Gargantext.Core
import Gargantext.Core.NLP (nlpServerGet)
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.Flow
import Gargantext.Database.Action.Search
import Gargantext.Database.Admin.Types.Hyperdata.Document
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Tree.Root
import Gargantext.Database.Schema.Node (NodePoly(..))
-- import Network.URI (parseURI)

import Test.Database.Types
import Test.Hspec.Expectations
import Test.Tasty.HUnit
import Gargantext.Core.Text.Terms.Mono.Stem
import Gargantext.Database.Admin.Config (userMaster)
import qualified Data.Text as T
import qualified Gargantext.Core.Text.Corpus.Query as API
import Gargantext.Database.Query.Facet


exampleDocument_01 :: HyperdataDocument
exampleDocument_01 = either error id $ parseEither parseJSON $ [aesonQQ|
  { "doi":"01"
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
  { "doi":""
  , "bdd": "Arxiv"
  , "publication_day":6
  , "language_iso2":"EN"
  , "publication_second":0
  , "authors":"Ajeje Brazorf, Manuel Agnelli"
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

exampleDocument_03 :: HyperdataDocument
exampleDocument_03 = either error id $ parseEither parseJSON $ [aesonQQ|
{
    "bdd": "Arxiv"
  , "doi": ""
  , "url": "http://arxiv.org/pdf/1405.3072v2"
  , "title": "Haskell for OCaml programmers"
  , "source": ""
  , "authors": "Raphael Poss, Herbert Ballerina"
  , "abstract": "  This introduction to Haskell is written to optimize learning by programmers who already know OCaml. "
  , "institutes": ""
  , "language_iso2": "EN"
  , "publication_date": "2014-05-13T09:10:32Z"
  , "publication_year": 2014
}
|]

exampleDocument_04 :: HyperdataDocument
exampleDocument_04 = either error id $ parseEither parseJSON $ [aesonQQ|
{
    "bdd": "Arxiv"
  , "doi": ""
  , "url": "http://arxiv.org/pdf/1407.5670v1"
  , "title": "Rust for functional programmers"
  , "source": ""
  , "authors": "Raphael Poss"
  , "abstract": "  This article provides an introduction to Rust , a systems language by Mozilla , to programmers already familiar with Haskell , OCaml or other functional languages. " , "institutes": ""
  , "language_iso2": "EN"
  , "publication_date": "2014-07-21T21:20:31Z"
  , "publication_year": 2014
}
|]

corpusAddDocuments :: TestEnv -> Assertion
corpusAddDocuments env = do
  flip runReaderT env $ runTestMonad $ do
    -- NOTE(adn) We need to create user 'gargantua'(!!) in order
    -- for 'addDocumentsToHyperCorpus' to work.
    parentId <- getRootId (UserName userMaster)
    [corpus] <- getCorporaWithParentId parentId
    let corpusId = _node_id corpus

    let lang = EN
    server <- view (nlpServerGet lang)
    ids <- addDocumentsToHyperCorpus server
                                     (Just $ _node_hyperdata $ corpus)
                                     (Multi lang)
                                     corpusId
                                     [exampleDocument_01, exampleDocument_02, exampleDocument_03, exampleDocument_04]
    liftIO $ length ids `shouldBe` 4

stemmingTest :: TestEnv -> Assertion
stemmingTest _env = do
  stem EN GargPorterAlgorithm "Ajeje"        `shouldBe` "Ajeje"
  stem EN GargPorterAlgorithm "PyPlasm:"     `shouldBe` "PyPlasm:"
  -- This test outlines the main differences between Porter and Lancaster.
  stem EN GargPorterAlgorithm "dancer"       `shouldBe` "dancer"
  stem EN LancasterAlgorithm  "dancer"       `shouldBe` "dant"
  stem EN GargPorterAlgorithm "postpartum"   `shouldBe` "postpartum"
  stem EN LancasterAlgorithm  "postpartum"   `shouldBe` "postpart"
  stem IT PorterAlgorithm     "catechizzare" `shouldBe` "catechizz"
  stem IT LancasterAlgorithm  "catechizzare" `shouldBe` "catechizzare" -- lancaster doesn't support Italian

mkQ :: T.Text -> API.Query
mkQ txt = either (\e -> error $ "(query) = " <> T.unpack txt <> ": " <> e) id . API.parseQuery . API.RawQuery $ txt

corpusSearch01 :: TestEnv -> Assertion
corpusSearch01 env = do
  flip runReaderT env $ runTestMonad $ do

    parentId <- getRootId (UserName userMaster)
    [corpus] <- getCorporaWithParentId parentId

    results1 <- searchInCorpus (_node_id corpus) False (mkQ "mineral") Nothing Nothing Nothing
    results2 <- searchInCorpus (_node_id corpus) False (mkQ "computational") Nothing Nothing Nothing

    liftIO $ length results1 `shouldBe` 1
    liftIO $ length results2 `shouldBe` 1

-- | Check that we support more complex queries
corpusSearch02 :: TestEnv -> Assertion
corpusSearch02 env = do
  flip runReaderT env $ runTestMonad $ do

    parentId <- getRootId (UserName userMaster)
    [corpus] <- getCorporaWithParentId parentId

    results1 <- searchInCorpus (_node_id corpus) False (mkQ "Raphael") Nothing Nothing Nothing
    results2 <- searchInCorpus (_node_id corpus) False (mkQ "Raphael Poss") Nothing Nothing Nothing

    liftIO $ do
      length results1 `shouldBe` 2 -- Haskell & Rust
      map facetDoc_title results2 `shouldBe` ["Haskell for OCaml programmers", "Rust for functional programmers"]

-- | Check that we support more complex queries via the bool API
corpusSearch03 :: TestEnv -> Assertion
corpusSearch03 env = do
  flip runReaderT env $ runTestMonad $ do

    parentId <- getRootId (UserName userMaster)
    [corpus] <- getCorporaWithParentId parentId

    results1 <- searchInCorpus (_node_id corpus) False (mkQ "\"Manuel Agnelli\"") Nothing Nothing Nothing
    results2 <- searchInCorpus (_node_id corpus) False (mkQ "Raphael AND -Rust") Nothing Nothing Nothing
    results3 <- searchInCorpus (_node_id corpus) False (mkQ "(Raphael AND (NOT Rust)) OR PyPlasm") Nothing Nothing Nothing

    liftIO $ do
      length results1 `shouldBe` 1
      map facetDoc_title results2 `shouldBe` ["Haskell for OCaml programmers"]
      map facetDoc_title results3 `shouldBe` ["PyPlasm: computational geometry made easy", "Haskell for OCaml programmers"]

-- | Check that the score doc count is correct
--   TODO This test is unfinished because `updateDocs` needs more work
corpusScore01 :: TestEnv -> Assertion
corpusScore01 env = do
  flip runReaderT env $ runTestMonad $ do

    parentId <- getRootId (UserName userMaster)
    [corpus] <- getCorporaWithParentId parentId

    results <- searchInCorpus (_node_id corpus) False (mkQ "Haskell") Nothing Nothing Nothing

    liftIO $ do
      map facetDoc_title results `shouldBe` ["Haskell for OCaml programmers", "Rust for functional programmers"]

      map facetDoc_score results `shouldBe` [Just 0.0, Just 0.0]

    -- _ <- updateDocs (_node_id corpus)

    liftIO $ do
      map facetDoc_score results `shouldBe` [Just 0.0, Just 0.0]

-- | Check that we support search with tsquery
corpusSearchDB01 :: TestEnv -> Assertion
corpusSearchDB01 env = do
  flip runReaderT env $ runTestMonad $ do

    parentId <- getRootId (UserName userMaster)
    [corpus] <- getCorporaWithParentId parentId

    results <- searchDocInDatabase (_node_id corpus) ("first second")

    liftIO $ do
      length results `shouldBe` 0 -- doesn't exist, we just check that proper to_tsquery is called
