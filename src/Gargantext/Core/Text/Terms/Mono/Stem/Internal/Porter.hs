{-# LANGUAGE LambdaCase #-}
module Gargantext.Core.Text.Terms.Mono.Stem.Internal.Porter
  ( stem ) where

import Prelude

import Data.Text qualified as T
import Gargantext.Core (Lang(..))
import NLP.Stemmer qualified as N

fromGargLang :: Lang -> N.Stemmer
fromGargLang = \case
  DE -> N.German
  EL -> N.Porter -- no greek specialised algo, defaults to 'Porter'
  EN -> N.English
  ES -> N.Spanish
  FR -> N.French
  IT -> N.Italian
  PL -> N.Porter -- no Polish specialised algo, defaults to 'Porter'
  PT -> N.Portuguese
  RU -> N.Russian
  UK -> N.Porter -- no Ukraine specialised algo, defaults to 'Porter'
  ZH -> N.Porter -- no chinese specialised algo, defaults to 'Porter'

stem :: Lang -> T.Text -> T.Text
stem lang = T.pack . N.stem (fromGargLang lang) . T.unpack
