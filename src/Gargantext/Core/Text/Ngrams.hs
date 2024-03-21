{-|
Module      : Gargantext.Core.Text.Ngrams
Description : Main Ngrams types
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Core.Text.Ngrams
  where

  
import Codec.Serialise (Serialise())
import Control.Lens (over)
import Data.Aeson ( ToJSON(..), FromJSON(..), FromJSONKey(..), FromJSONKeyFunction(..), Value(String), ToJSONKey(..) )
import Data.Aeson.Types (toJSONKeyText)
import Data.Text (pack)
import Database.PostgreSQL.Simple qualified as PGS
import Gargantext.Core.Types (TODO(..))
import Gargantext.Database.Schema.Prelude hiding (over)
import Gargantext.Prelude
import Servant (FromHttpApiData(..), ToHttpApiData(..))
import Test.QuickCheck (elements)
import Text.Read (read)


-- | Main Ngrams Types
-- | Typed Ngrams
-- Typed Ngrams localize the context of the ngrams
-- ngrams in source  field  of document  has Sources Type
-- ngrams in authors field  of document  has Authors Type
-- ngrams in text    fields of documents has Terms   Type (i.e. either title or abstract)
data NgramsType = Authors | Institutes | Sources | NgramsTerms
  deriving (Eq, Show, Read, Ord, Enum, Bounded, Generic)

instance Serialise NgramsType
instance FromJSON NgramsType
  where
    parseJSON (String "Authors")     = pure Authors
    parseJSON (String "Institutes")  = pure Institutes
    parseJSON (String "Sources")     = pure Sources
    parseJSON (String "Terms")       = pure NgramsTerms
    parseJSON (String "NgramsTerms") = pure NgramsTerms
    parseJSON _                      = mzero

instance FromJSONKey NgramsType where
   fromJSONKey = FromJSONKeyTextParser (parseJSON . String)

instance ToJSON NgramsType
  where
    toJSON Authors     = String "Authors"
    toJSON Institutes  = String "Institutes"
    toJSON Sources     = String "Sources"
    toJSON NgramsTerms = String "Terms"

instance ToJSONKey NgramsType where
   toJSONKey = toJSONKeyText (pack . show)
instance FromHttpApiData NgramsType where
  parseUrlPiece n = pure $ (read . cs) n
instance ToHttpApiData NgramsType where
  toUrlPiece = pack . show
instance ToParamSchema NgramsType where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)
instance Arbitrary NgramsType where
  arbitrary = elements [ minBound .. maxBound ]


ngramsTypes :: [NgramsType]
ngramsTypes = [minBound..]

instance ToSchema NgramsType
{-  where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_nre_")
--}


    
data Ngrams = UnsafeNgrams { _ngramsTerms :: Text
                           , _ngramsSize  :: Int
                           }
  deriving (Generic, Show, Eq, Ord)

instance Hashable Ngrams

makeLenses ''Ngrams
instance PGS.ToRow Ngrams where
  toRow (UnsafeNgrams t s) = [toField t, toField s]

------------------------------------------------------------------------
-------------------------------------------------------------------------
-- Named entity are typed ngrams of Terms Ngrams
data NgramsT a =
  NgramsT { _ngramsType :: NgramsType
          , _ngramsT    :: a
          } deriving (Generic, Show, Eq, Ord)

makeLenses ''NgramsT

instance Functor NgramsT where
  fmap = over ngramsT

