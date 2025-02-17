{-|
Module      : Gargantext.Types
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DerivingStrategies   #-}

module Gargantext.Core.Types ( module Gargantext.Core.Types.Main
                             , module Gargantext.Database.Admin.Types.Node
                             , DebugMode(..), withDebugMode
                             , Term(..), Terms(..), TermsCount, TermsWithCount
                             , TokenTag(..), POS(..), NER(..)
                             , Label, Stems
                             , HasValidationError(..), assertValid
                             , Name
                             , TableResult(..), NodeTableResult
                             , Ordering(..)
                             , Typed(..), withType , unTyped
                             , TODO(..)
                             ) where

import Control.Lens (Prism', (#), makeLenses, over)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Data.Set (empty)
import Data.String
import Data.Swagger (ToParamSchema)
import Data.Swagger (ToSchema(..))
import Data.Text (unpack)
import Data.Validity
import GHC.Generics
import Gargantext.Core.Types.Main
import Gargantext.Core.Utils.Prefix (unPrefix, wellNamedSchema)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Prelude hiding (Ordering, empty)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

------------------------------------------------------------------------

data DebugMode = DebugMode { activated :: Bool }

withDebugMode :: (Show a) => DebugMode -> Text -> a -> b -> b
withDebugMode (DebugMode True ) msg var a = trace ("DEBUG" <> msg <> (show var) :: Text) a
withDebugMode (DebugMode False) _   _ a = a

------------------------------------------------------------------------
data Ordering = Down | Up
  deriving (Enum, Show, Eq, Bounded)

------------------------------------------------------------------------
type Name = Text

newtype Term = Term { getTerm :: Text }
  deriving newtype (Eq, Ord, IsString, Show)

type Stems = Set Text
type Label = [Text]

data Terms = Terms { _terms_label :: Label
                   , _terms_stem  :: Stems
                   } deriving (Ord, Show)
instance Eq Terms where
  (==) (Terms _ s1) (Terms _ s2) = s1 == s2

type TermsCount = Int

type TermsWithCount = (Terms, TermsCount)

------------------------------------------------------------------------
data Tag = POS | NER
  deriving (Show, Eq)
------------------------------------------------------------------------
data POS = NP
         | JJ  | VB
         | CC  | IN | DT
         | ADV
         | NotFound { not_found :: [Char] }
  deriving (Show, Generic, Eq, Ord)
------------------------------------------------------------------------
-- https://pythonprogramming.net/part-of-speech-tagging-nltk-tutorial/
instance FromJSON POS where
  parseJSON = withText "String" (\x -> pure (pos $ unpack x))
    where
      pos :: [Char] -> POS
      pos "ADJ"  = JJ
      pos "CC"   = CC
      pos "CCONJ"= CC
      pos "DT"   = DT
      pos "DET"  = DT
      pos "IN"   = IN
      pos "JJ"    = JJ
      pos "PROPN" = JJ
      pos "JJR"  = JJ
      pos "JJS"  = JJ
      pos "NC"   = NP
      pos "NN"   = NP
      pos "NOUN" = NP
      pos "NNS"  = NP
      pos "NNP"  = NP
      pos "NNPS" = NP
      pos "NP"   = NP
      pos "VB"   = VB
      pos "VERB" = VB
      pos "VBD"  = VB
      pos "VBG"  = VB
      pos "VBN"  = VB
      pos "VBP"  = VB
      pos "VBZ"  = VB
      pos "RB"   = ADV
      pos "ADV"  = ADV
      pos "RBR"  = ADV
      pos "RBS"  = ADV
      pos "WRB"  = ADV
      -- French specific
      pos "P"     = IN
      pos "PUNCT" = IN
      pos  x      = NotFound x

instance ToJSON POS
instance Hashable POS
------------------------------------------------------------------------
data NER = PERSON | ORGANIZATION | LOCATION | NoNER { noNer :: !Text }
  deriving (Show, Generic)
------------------------------------------------------------------------
instance FromJSON NER where
  parseJSON = withText "String" (\x -> pure (ner $ unpack x))
    where
      ner :: [Char] -> NER
      ner "PERSON"       = PERSON
      ner "PER"          = PERSON
      ner "ORGANIZATION" = ORGANIZATION
      ner "LOCATION"     = LOCATION
      ner "LOC"          = LOCATION
      ner  x             = NoNER (cs x)

instance ToJSON NER

data TokenTag  = TokenTag { _my_token_word  :: [Text]
                          , _my_token_lemma :: Set Text
                          , _my_token_pos   :: Maybe POS
                          , _my_token_ner   :: Maybe NER
                          } deriving (Show)

instance Semigroup TokenTag where
  (<>) (TokenTag w1 s1 p1 n1) (TokenTag w2 s2 p2 _) = TokenTag (w1 <> w2) (s1 <> s2) p3 n1
    where
      p3 = case (p1,p2) of
        (Just JJ, Just NP) -> Just NP
        (Just VB, Just NP) -> Just NP
        _                  -> p1


instance Monoid TokenTag where
  mempty = TokenTag [] empty Nothing Nothing
  mconcat = foldl' mappend mempty
  -- mappend t1 t2 = (<>) t1 t2


class HasValidationError e where
  _ValidationError :: Prism' e Validation

assertValid :: (MonadError e m, HasValidationError e) => Validation -> m ()
assertValid v = when (not $ validationIsValid v) $ throwError $ _ValidationError # v
-- assertValid :: MonadBase IO m => Validation -> m ()
-- assertValid v = when (not $ validationIsValid v) $ fail $ show v

-- | NodeTableResult (Table computations)
type NodeTableResult a = TableResult (Node a)


data TableResult a = TableResult { tr_count :: Int
                                 , tr_docs  :: [a]
                                 } deriving (Generic, Show)

$(deriveJSON (unPrefix "tr_") ''TableResult)

instance (Typeable a, ToSchema a) => ToSchema (TableResult a) where
  declareNamedSchema = wellNamedSchema "tr_"

instance Arbitrary a => Arbitrary (TableResult a) where
  arbitrary = TableResult <$> arbitrary <*> arbitrary

----------------------------------------------------------------------------
data Typed a b =
  Typed { _withType :: a
        , _unTyped  :: b
        }
  deriving (Generic, Show, Eq, Ord)

makeLenses ''Typed

instance Functor (Typed a) where
  fmap = over unTyped

----------------------------------------------------------------------------
-- TO BE removed
data TODO = TODO
  deriving (Generic)

instance ToSchema TODO where
instance ToParamSchema TODO where
----------------------------------------------------------------------------
