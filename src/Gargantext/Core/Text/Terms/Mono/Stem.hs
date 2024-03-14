{-|
Module      : Gargantext.Core.Text.Terms.Mono.Stem
Description : Stemming of mono (i.e. single word) terms.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

In linguistic morphology and information retrieval, stemming is the
process of reducing inflected (or sometimes derived) words to their word
stem, base or root form—generally a written word form. The @stem@ needs
not be identical to the morphological root of the word; it is usually
sufficient that related words map to the same stem, even if this stem is
not in itself a valid root.
Source : https://en.wikipedia.org/wiki/Stemming
A stemmer for English, for example, should identify the string "cats"
(and possibly "catlike", "catty" etc.) as based on the root "cat", and
"stems", "stemmer", "stemming", "stemmed" as based on "stem". A stemming
algorithm reduces the words "fishing", "fished", and "fisher" to the
root word, "fish". On the other hand, "argue", "argued", "argues",
"arguing", and "argus" reduce to the stem "argu" (illustrating the
case where the stem is not itself a word or root) but "argument" and
"arguments" reduce to the stem "argument".

-}


module Gargantext.Core.Text.Terms.Mono.Stem (

  -- * Types
  StemmingAlgorithm(..),

  -- * Universal stemming function
  stem,

  -- * Handy re-exports
  Lang(..)

  ) where

import Gargantext.Core.Text.Terms.Mono.Stem.Internal.Porter qualified as Porter
import Gargantext.Core.Text.Terms.Mono.Stem.Internal.Lancaster qualified as Lancaster
import Gargantext.Core.Text.Terms.Mono.Stem.Internal.GargPorter qualified as GargPorter
import Gargantext.Core (Lang(..))
import Gargantext.Prelude

-- | A stemming algorithm. There are different stemming algorithm,
-- each with different tradeoffs, strengths and weaknesses. Typically
-- one uses one or the other based on the given task at hand.
data StemmingAlgorithm
  = -- | The porter algorithm is the classic stemming algorithm, possibly
    -- one of the most widely used.
    PorterAlgorithm
    -- | Slight variation of the porter algorithm; it's more aggressive with
    -- stemming, which might or might not be what you want. It also makes some
    -- subtle chances to the stem; for example, the stemming of \"dancer\" using
    -- Porter is simply \"dancer\" (i.e. it cannot be further stemmed). Using
    -- Lancaster we would get \"dant\", which is not a prefix of the initial word anymore.
  | LancasterAlgorithm
    -- | A variation of the Porter algorithm tailored for Gargantext.
  | GargPorterAlgorithm
  deriving (Show, Eq, Ord)

-- | Stems the input 'Text' based on the input 'Lang' and using the
-- given 'StemmingAlgorithm'.
stem :: Lang -> StemmingAlgorithm -> Text -> Text
stem lang algo unstemmed = case algo of
  PorterAlgorithm
    -> Porter.stem lang unstemmed
  LancasterAlgorithm
    | EN <- lang
    -> Lancaster.stem unstemmed
    | otherwise
    -> unstemmed -- Lancaster doesn't support any other language which is not english.
  GargPorterAlgorithm
    | EN <- lang
    -> GargPorter.stem unstemmed
    | otherwise
    -> unstemmed -- Our garg porter doesn't support other languages other than english.
