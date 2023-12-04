{-|
Module      : Gargantext.Core.Text.Terms.WithList
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.

-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE ViewPatterns      #-}

module Gargantext.Core.Text.Terms.WithList where

import Data.Algorithms.KMP qualified as KMP
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.Ord
import Data.Text (concat)
import Data.Text qualified as Text
import Gargantext.API.Ngrams.Types (NgramsTerm(..))
import Gargantext.Core (Lang(ZH))
import Gargantext.Core.Text.Context
import Gargantext.Core.Text.Terms.Mono (monoTextsBySentence)
import Gargantext.Core.Types (TermsCount)
import Gargantext.Core.Utils (groupWithCounts)
import Gargantext.Prelude hiding (concat)
import GHC.Exts (sortWith)

------------------------------------------------------------------------

data Pattern = Pattern
  { _pat_table  :: !(KMP.Table Text)
  , _pat_length :: !Int
  , _pat_terms  :: ![Text]
  }
type Patterns = [Pattern]

------------------------------------------------------------------------

data ReplaceTerms = KeepAll | LongestOnly

replaceTerms :: ReplaceTerms -> Patterns -> [Text] -> [[Text]]
replaceTerms rplaceTerms pats terms = go 0
  where
    terms_len = length terms

    go ix | ix >= terms_len = []
          | otherwise =
      case IntMap.lookup ix m of
        Nothing -> go (ix + 1)
        Just (len, term) ->
          term : go (ix + len)

    m = toMap
        [ (ix, (len, term))
        | Pattern pat len term <- pats, ix <- KMP.match pat terms ]

    toMap = case rplaceTerms of
      KeepAll -> IntMap.fromList
      LongestOnly -> IntMap.fromListWith merge
        where
          merge (len1, lab1) (len2, lab2) =
            if len2 < len1 then (len1, lab1) else (len2, lab2)

buildPatternsWith :: Lang -> [NgramsTerm] -> Patterns
buildPatternsWith ZH ts = buildPatterns $ map (\k -> (Text.chunksOf 1  $ unNgramsTerm k, [])) ts
buildPatternsWith _  ts = buildPatterns $ map (\k -> (Text.splitOn " " $ unNgramsTerm k, [])) ts

buildPatterns :: TermList -> Patterns
buildPatterns = sortWith (Down . _pat_length) . concatMap buildPattern
  where
    buildPattern (label, alts) = map f $ map (\alt -> filter (/= "") alt) (label : alts)
      where
        f alt | "" `elem` alt = errorTrace ("buildPatterns: ERR1" <> show(label))
              | null alt      = errorTrace "buildPatterns: ERR2"
              | otherwise     =
                Pattern (KMP.build alt) (length alt) label
                        --(Terms label $ Set.empty) -- TODO check stems


--------------------------------------------------------------------------
-- Utils
type MatchedText = Text

termsInText :: Lang -> Patterns -> Text -> [(MatchedText, TermsCount)]
termsInText lang pats (manipulateText lang -> txt) =
  groupWithCounts $ List.concat
                  $ map (map unwords)
                  $ extractTermsWithList pats txt

-- | Manipulates the input 'Text' before passing it to 'termsInText'.
-- In particular, if the language is Chinese (ZH), we add spaces.
manipulateText :: Lang -> Text -> Text
manipulateText lang txt = case lang of
  ZH -> addSpaces txt
  _  -> txt

--------------------------------------------------------------------------
extractTermsWithList :: Patterns -> Text -> Corpus [Text]
extractTermsWithList pats = map (replaceTerms KeepAll pats) . monoTextsBySentence

-- | Extract terms
-- >>> let termList = [(["chat blanc"], [["chat","blanc"]])] :: TermList
-- extractTermsWithList' (buildPatterns termList) "Le chat blanc"["chat blanc"]
-- ["chat blanc"]
extractTermsWithList' :: Patterns -> Text -> [Text]
extractTermsWithList' pats = map (concat . map concat . replaceTerms KeepAll pats)
                           . monoTextsBySentence

--------------------------------------------------------------------------
addSpaces :: Text -> Text
addSpaces = (Text.intercalate " ") . (Text.chunksOf 1)


--------------------------------------------------------------------------

{- | Not used
filterWith :: TermList
           -> (a -> Text)
           -> [a]
           -> [(a, [Text])]
filterWith termList f xs = filterWith' termList f zip xs


filterWith' :: TermList
           -> (a -> Text)
           -> ([a] -> [[Text]] -> [b])
           -> [a]
           -> [b]
filterWith' termList f f' xs = f' xs
                            $ map (extractTermsWithList' pats)
                            $ map f xs
    where
      pats = buildPatterns termList
-}
