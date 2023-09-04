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
import Gargantext.Database.Action.User
import Gargantext.Database.Action.User.New
import Gargantext.Database.Admin.Types.Hyperdata.Corpus
import Gargantext.Database.Admin.Types.Hyperdata.Document
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Tree.Root
import Gargantext.Database.Schema.Node (NodePoly(..))
import Network.URI (parseURI)

import Test.Tasty.HUnit
import Database.Operations.Types


exampleDocument_01 :: HyperdataDocument
exampleDocument_01 = either error id $ parseEither parseJSON $ [aesonQQ|
  { "doi":"sdfds"
  , "publication_day":6
  , "language_iso2":"en"
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
  , "language_iso2":"en"
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
    let nur = mkNewUser "gargantua@foo.com" (GargPassword "my_secret")
    void $ new_user nur
    uid      <- getUserId (UserName "alfredo")
    parentId <- getRootId (UserName "gargantua")
    void $ mk (Just "Test_Corpus") (Nothing :: Maybe HyperdataCorpus) parentId uid
    [corpus] <- getCorporaWithParentId parentId

    _ids <- addDocumentsToHyperCorpus nlpServerConfig
                                      (Just $ _node_hyperdata $ corpus)
                                      (Multi EN)
                                      (_node_id corpus)
                                      [exampleDocument_01]
    pure ()
