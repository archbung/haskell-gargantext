{-|
Module      : Gargantext.API.Count
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Count API part of Gargantext.
-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Gargantext.API.Count
      where

import Data.Aeson hiding (Error)
import Data.Aeson.TH (deriveJSON)
import Data.Either
import Data.Swagger
import Data.Text (pack)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Prelude
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

-----------------------------------------------------------------------
-- TODO-ACCESS: CanCount
-- TODO-EVENTS: No events as this is a read only query.
type CountAPI = Post '[JSON] Counts

-----------------------------------------------------------------------
data Scraper = Pubmed | Hal | IsTex | Isidore
  deriving (Eq, Show, Generic, Enum, Bounded)

scrapers :: [Scraper]
scrapers = [minBound..maxBound]

instance FromJSON Scraper
instance ToJSON   Scraper

instance Arbitrary Scraper where
    arbitrary = elements scrapers

instance ToSchema Scraper

-----------------------------------------------------------------------
data QueryBool = QueryBool Text
        deriving (Eq, Show, Generic)

queries :: [QueryBool]
queries =  [QueryBool (pack "(X OR X') AND (Y OR Y') NOT (Z OR Z')")]
--queries =  [QueryBool (pack "(X + X') * (Y + Y') - (Z + Z')")]

instance Arbitrary QueryBool where
    arbitrary = elements queries

instance FromJSON QueryBool
instance ToJSON   QueryBool

instance ToSchema QueryBool
-----------------------------------------------------------------------

data Query = Query { query_query :: QueryBool
                   , query_name  :: Maybe [Scraper]
                   }
                   deriving (Eq, Show, Generic)
instance FromJSON Query
instance ToJSON   Query

instance Arbitrary Query where
    arbitrary = elements [ Query q (Just n) 
                         | q <- queries
                         , n <- take 10 $ permutations scrapers
                         ]

instance ToSchema Query where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "query_")

-----------------------------------------------------------------------
type Code = Integer
type Error  = Text
type Errors = [Error]

-----------------------------------------------------------------------
data Message = Message Code Errors
        deriving (Eq, Show, Generic)

toMessage :: [(Code, Errors)] -> [Message]
toMessage = map (\(c,err) -> Message c err)

messages :: [Message]
messages =  toMessage $ [ (400, ["Ill formed query             "])
                        , (300, ["API connexion error          "])
                        , (300, ["Internal Gargantext Error    "])
                        ] <> take 10 ( repeat (200, [""]))

instance Arbitrary Message where
    arbitrary = elements messages

instance ToSchema Message
-----------------------------------------------------------------------
data Counts = Counts { results :: [Either Message Count]
                     } deriving (Eq, Show, Generic)


instance Arbitrary Counts where
    arbitrary = elements [Counts [ Right (Count Pubmed (Just 20 ))
                                 , Right (Count IsTex  (Just 150))
                                 , Right (Count Hal    (Just 150))
                                 ]
                         ]

instance ToSchema Counts

-----------------------------------------------------------------------
data Count = Count { count_name    :: Scraper
                   , count_count   :: Maybe Int
                   }
                   deriving (Eq, Show, Generic)

instance ToSchema Count where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "count_")
--instance Arbitrary Count where
--    arbitrary = Count <$> arbitrary <*> arbitrary <*> arbitrary

-----------------------------------------------------------------------
count :: Monad m => Query -> m Counts
count _ = undefined

--
-- JSON instances
--

instance FromJSON Message
instance ToJSON   Message

$(deriveJSON (unPrefix "count_") ''Count)

instance FromJSON Counts
instance ToJSON   Counts

