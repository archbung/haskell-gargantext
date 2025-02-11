{-|
Module      : Gargantext.Core.Text.Clean
Description : Tools to clean text
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Clean some texts before importing it.

For a given Language, chose a big master piece of litteracy to analyze
it with GarganText. Here is a an example with a famous French Writer
that could be the incarnation of the mythic Gargantua.

-}

{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Core.Text.Prepare
  where

import Data.List qualified as List
import Data.Text qualified as Text
import Gargantext.Core.Text (sentences)
import Gargantext.Prelude


---------------------------------------------------------------------
prepareText :: Paragraph -> Text -> [Text]
prepareText p txt = groupText p
                  $ List.filter (/= "")
                  $ toParagraphs
                  $ Text.lines
                  $ Text.replace "_" " "  -- some texts seem to be underlined
                  $ Text.replace "--" ""  -- removing bullets like of dialogs
                  $ Text.replace "\xd" "" txt

---------------------------------------------------------------------

groupText :: Paragraph -> [Text] -> [Text]
groupText (Uniform blockSize) = groupUniform blockSize
groupText AuthorLike          = groupLines

---------------------------------------------------------------------
data Paragraph = Uniform Grain | AuthorLike
-- Uniform does not preserve the paragraphs of the author but length of paragraphs is uniform
-- Author Like preserve the paragraphs of the Author but length of paragraphs is not uniform

-- Grain: number of Sentences by block of Text
-- Step : overlap of sentence between connex block of Text
groupUniform :: Grain -> [Text] -> [Text]
groupUniform g ts = map (Text.intercalate " ")
                  $ chunkAlong g g
                  $ sentences
                  $ Text.concat ts

groupLines :: [Text] -> [Text]
groupLines xxx@(a:b:xs) = 
  if Text.length a > moyenne
     then [a] <> (groupLines (b:xs))
     else let ab = a <> " " <> b in
              if Text.length ab > moyenne
                then [ab] <> (groupLines xs)
                else groupLines ([ab] <> xs)
  where
    moyenne = round
            $ mean
            $ (map (fromIntegral . Text.length) xxx :: [Double])
groupLines [a] = [a]
groupLines [] = []

groupLines_test :: [Text]
groupLines_test = groupLines theData
  where
    theData = ["abxxxx", "bc", "cxxx", "d"]

---------------------------------------------------------------------
toParagraphs :: [Text] -> [Text]
toParagraphs (a:x:xs) =
  if a == ""
     then [a] <> toParagraphs (x:xs)
     else if x == ""
            then [a] <> toParagraphs (x:xs)
            else toParagraphs $ [a <> " " <> x ] <> xs
toParagraphs [a] = [a]
toParagraphs [] = []

-- Tests

-- TODO for internships: Property tests
toParagraphs_test :: Bool
toParagraphs_test = 
  toParagraphs ["a","b","","c","d","d","","e","f","","g","h",""]
     == [ "a b", "", "c d d", "", "e f", "", "g h", ""]





