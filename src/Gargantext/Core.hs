{-|
Module      : Gargantext.Core
Description : Supported Natural language
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.Core
  where

import Data.Aeson
import Data.LanguageCodes qualified as ISO639
import Data.Bimap qualified as Bimap
import Data.Bimap (Bimap)
import Data.Morpheus.Types (GQLType)
import Data.Swagger
import Data.Text (pack)
import Gargantext.Prelude hiding (All)
import Servant.API
import Test.QuickCheck

------------------------------------------------------------------------
-- | Language of a Text
-- For simplicity, we suppose text has an homogenous language
--
--  - EN == english
--  - FR == french
--  - DE == deutch
--  - IT == italian
--  - ES == spanish
--  - PL == polish
--  - ZH == chinese
--
--  ... add your language and help us to implement it (:

-- | All languages supported
-- NOTE: Use international country codes
-- https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
-- TODO This should be deprecated in favor of iso-639 library
data Lang = All
          | DE
          | EL
          | EN
          | ES
          | FR
          | IT
          | PL
          | PT
          | RU
          | UK
          | ZH
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, GQLType)

-- | Defaults to 'EN' in all those places where a language is mandatory,
-- but an optional one has been passed.
withDefaultLanguage :: Maybe Lang -> Lang
withDefaultLanguage = fromMaybe defaultLanguage

-- | The default 'Lang'.
defaultLanguage :: Lang
defaultLanguage = EN

instance ToJSON Lang
instance FromJSON Lang
instance ToSchema Lang where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance FromHttpApiData Lang
  where
    parseUrlPiece "All" = pure All
    parseUrlPiece "DE"  = pure DE
    parseUrlPiece "EL"  = pure EL
    parseUrlPiece "EN"  = pure EN
    parseUrlPiece "ES"  = pure ES
    parseUrlPiece "FR"  = pure FR
    parseUrlPiece "IT"  = pure IT
    parseUrlPiece "PL"  = pure PL
    parseUrlPiece "PT"  = pure PT
    parseUrlPiece "RU"  = pure RU
    parseUrlPiece "UK"  = pure UK
    parseUrlPiece "ZH"  = pure ZH
    parseUrlPiece _     = Left "Unexpected value of Lang"
instance ToHttpApiData Lang where
  toUrlPiece = pack . show
instance Hashable Lang
instance Arbitrary Lang where
  arbitrary = arbitraryBoundedEnum

toISO639 :: Lang -> Maybe ISO639.ISO639_1
toISO639 DE = Just ISO639.DE
toISO639 EL = Just ISO639.EL
toISO639 EN = Just ISO639.EN
toISO639 ES = Just ISO639.ES
toISO639 FR = Just ISO639.FR
toISO639 IT = Just ISO639.IT
toISO639 PL = Just ISO639.PL
toISO639 PT = Just ISO639.PT
toISO639 RU = Just ISO639.RU
toISO639 UK = Just ISO639.UK
toISO639 ZH = Just ISO639.ZH
toISO639 All = Nothing

toISO639EN :: Lang -> ISO639.ISO639_1
toISO639EN l = fromMaybe ISO639.EN $ toISO639 l

iso639ToText :: ISO639.ISO639_1 -> Text
iso639ToText la = pack [a, b]
  where
    (a, b) = ISO639.toChars la

-- | https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
toISO639Lang :: Lang -> Maybe Text
toISO639Lang All = Nothing
toISO639Lang DE = Just "de"
toISO639Lang EL = Just "el"
toISO639Lang EN = Just "en"
toISO639Lang ES = Just "es"
toISO639Lang FR = Just "fr"
toISO639Lang IT = Just "it"
toISO639Lang PL = Just "pl"
toISO639Lang PT = Just "pt"
toISO639Lang RU = Just "ru"
toISO639Lang UK = Just "uk"
toISO639Lang ZH = Just "zh"

allLangs :: [Lang]
allLangs = [minBound .. maxBound]

class HasDBid a where
  toDBid     :: a   -> Int
  lookupDBid :: Int -> Maybe a

-- NOTE: We try to use numeric codes for countries
-- https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
-- https://en.wikipedia.org/wiki/ISO_3166-1_numeric#004
-- The pattern matching ensures this mapping will always be total
-- once we add a new 'Lang'.
langIds :: Bimap Lang Int
langIds = Bimap.fromList $ allLangs <&> \lid -> case lid of
  All -> (lid, 0)
  DE  -> (lid, 276)
  EL  -> (lid, 300)
  EN  -> (lid, 2)
  ES  -> (lid, 724)
  FR  -> (lid, 1)
  IT  -> (lid, 380)
  PL  -> (lid, 616)
  PT  -> (lid, 620)
  RU  -> (lid, 643)
  UK  -> (lid, 804)
  ZH  -> (lid, 156)

instance HasDBid Lang where
  -- /NOTE/ this lookup cannot fail because 'dbIds' is defined as a total function
  -- over its domain.
  toDBid lang     = langIds Bimap.! lang
  lookupDBid dbId = Bimap.lookupR dbId langIds

------------------------------------------------------------------------
data NLPServerConfig = NLPServerConfig
  { server :: !PosTagAlgo
  , url    :: !URI }
  deriving (Show, Eq, Generic)
------------------------------------------------------------------------
type Form = Text
type Lem  = Text
------------------------------------------------------------------------
data PosTagAlgo = CoreNLP | JohnSnowServer | Spacy
  deriving (Show, Read, Eq, Ord, Generic, GQLType)

instance Hashable PosTagAlgo

instance HasDBid PosTagAlgo where
  toDBid CoreNLP        = 1
  toDBid JohnSnowServer = 2
  toDBid Spacy          = 3
  lookupDBid 1          = Just CoreNLP
  lookupDBid 2          = Just JohnSnowServer
  lookupDBid 3          = Just Spacy
  lookupDBid _          = Nothing


-- | Tries to convert the given integer into the relevant DB identifier, failing
-- with an error if the conversion cannot be performed.
fromDBid :: forall a. (HasCallStack, HasDBid a, Typeable a) => Int -> a
fromDBid i = case lookupDBid i of
  Nothing -> panic $ "HasDBid " <> show (typeRep (Proxy :: Proxy a)) <> " not found or not implemented."
  Just v  -> v
