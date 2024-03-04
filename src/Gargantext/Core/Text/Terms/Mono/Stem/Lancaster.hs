{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Core.Text.Terms.Mono.Stem.Lancaster
  ( stemIt
  ) where

import Prelude

import Data.Text (Text)
import qualified Data.Text as T

data Rule = Rule
    { _match :: Text
    , _replacement :: Text
    , _ruleType :: RuleType
    } deriving (Show, Eq)

data RuleType
  = Intact
  | Continue
  | Contint
  | Stop
  | Protect
  deriving (Show, Eq)

type RuleCollection = [(Char, [Rule])]

stop, intact, cont, protect, contint :: RuleType
stop    = Stop
intact  = Intact
cont    = Continue
protect = Protect
contint = Contint

-- Define rules
rulesPaper :: RuleCollection
rulesPaper =
    [ ('a', [ Rule "ia" "" intact, Rule "a" "" intact ])
    , ('b', [ Rule "bb" "b" stop ])
    , ('c', [ Rule "ytic" "ys" stop, Rule "ic" "" cont, Rule "nc" "nt" cont ])
    , ('d', [ Rule "dd" "d" stop, Rule "ied" "i" stop, Rule "ceed" "cess" stop, Rule "eed" "ee" stop
             , Rule "ed" "" cont, Rule "hood" "" cont ])
    , ('e', [ Rule "e" "" cont ])
    , ('f', [ Rule "lief" "liev" stop, Rule "if" "" cont ])
    , ('g', [ Rule "ing" "" cont, Rule "iag" "y" stop, Rule "ag" "" cont, Rule "gg" "g" stop ])
    , ('h', [ Rule "th" "" intact, Rule "guish" "ct" stop, Rule "ish" "" cont ])
    , ('i', [ Rule "i" "" intact, Rule "i" "y" cont ])
    , ('j', [ Rule "ij" "id" stop, Rule "fuj" "fus" stop, Rule "uj" "ud" stop, Rule "oj" "od" stop
             , Rule "hej" "her" stop, Rule "verj" "vert" stop, Rule "misj" "mit" stop, Rule "nj" "nd" stop
             , Rule "j" "s" stop ])
    , ('l', [ Rule "ifiabl" "" stop, Rule "iabl" "y" stop, Rule "abl" "" cont, Rule "ibl" "" stop
             , Rule "bil" "bl" cont, Rule "cl" "c" stop, Rule "iful" "y" stop, Rule "ful" "" cont
             , Rule "ul" "" stop, Rule "ial" "" cont, Rule "ual" "" cont, Rule "al" "" cont
             , Rule "ll" "l" stop ])
    , ('m', [ Rule "ium" "" stop, Rule "um" "" intact, Rule "ism" "" cont, Rule "mm" "m" stop ])
    , ('n', [ Rule "sion" "j" cont, Rule "xion" "ct" stop, Rule "ion" "" cont, Rule "ian" "" cont
             , Rule "an" "" cont, Rule "een" "" protect, Rule "en" "" cont, Rule "nn" "n" stop ])
    , ('p', [ Rule "ship" "" cont, Rule "pp" "p" stop ])
    , ('r', [ Rule "er" "" cont, Rule "ear" "" protect, Rule "ar" "" stop, Rule "or" "" cont
             , Rule "ur" "" cont, Rule "rr" "r" stop, Rule "tr" "t" cont, Rule "ier" "y" cont ])
    , ('s', [ Rule "ies" "y" cont, Rule "sis" "s" stop, Rule "is" "" cont, Rule "ness" "" cont
             , Rule "ss" "" protect, Rule "ous" "" cont, Rule "us" "" intact, Rule "s" "" contint
             , Rule "s" "" protect ])
    , ('t', [ Rule "plicat" "ply" stop, Rule "at" "" cont, Rule "ment" "" cont, Rule "ent" "" cont
             , Rule "ant" "" cont, Rule "ript" "rib" stop, Rule "orpt" "orb" stop, Rule "duct" "duc" stop
             , Rule "sumpt" "sum" stop, Rule "cept" "ceiv" stop, Rule "olut" "olv" stop
             , Rule "sist" "" protect, Rule "ist" "" cont, Rule "tt" "t" stop ])
    , ('u', [ Rule "iqu" "" stop, Rule "ogu" "og" stop ])
    , ('v', [ Rule "siv" "j" cont, Rule "eiv" "" protect, Rule "iv" "" cont ])
    , ('y', [ Rule "bly" "bl" cont, Rule "ily" "y" cont, Rule "ply" "" protect, Rule "ly" "" cont
             , Rule "ogy" "og" stop, Rule "phy" "ph" stop, Rule "omy" "om" stop, Rule "opy" "op" stop
             , Rule "ity" "" cont, Rule "ety" "" cont, Rule "lty" "l" stop, Rule "istry" "" stop
             , Rule "ary" "" cont, Rule "ory" "" cont, Rule "ify" "" stop, Rule "ncy" "nt" cont
             , Rule "acy" "" cont ])
    , ('z', [ Rule "iz" "" cont, Rule "yz" "ys" stop ])
    ]

-- Returns 'True' if the input character is a vowel.
isVowel :: Char -> Bool
isVowel c = c `elem` vowelsSet
{-# INLINE isVowel #-}

vowelsSet :: String
vowelsSet = "aeiouy"
{-# INLINE vowelsSet #-}

stemIt :: Text -> Text
stemIt inputText = lancasterStemmer inputText rulesPaper

-- Lancaster Stemmer
lancasterStemmer :: Text -> RuleCollection -> Text
lancasterStemmer inputText rules = applyRules (T.toLower inputText) True rules

applyRules :: Text -> Bool -> RuleCollection -> Text
applyRules value isIntact rules =
    case T.unsnoc value of
        Nothing -> value
        Just (_, lastChar) ->
            case lookup lastChar rules of
                Nothing -> value
                Just ruleset -> applyRuleSet value isIntact ruleset
  where
    applyRuleSet :: Text -> Bool -> [Rule] -> Text
    applyRuleSet val _ [] = val
    applyRuleSet val isIntact' (rule:rest) =
        case ruleApplication value isIntact' rule of
            Just res -> res
            Nothing -> applyRuleSet val isIntact' rest

    ruleApplication :: Text -> Bool -> Rule -> Maybe Text
    ruleApplication val isIntact' (Rule m r t) =
        if not isIntact' && (t == intact || t == contint)
            then Nothing
            else case T.stripSuffix m val of
                    Nothing -> Nothing
                    Just stem ->
                        let next = stem `T.append` r
                        in if not (acceptable next)
                            then Nothing
                            else if t == cont || t == contint
                                then Just $ applyRules next False rules
                                else Just next

-- | Returns 'True' if a stem is acceptable.
acceptable :: Text -> Bool
acceptable val
  | T.null val = False
  | otherwise
  = if isVowel (T.head val)
        then T.length val > 1
        else T.length val > 2 && T.any isVowel val
