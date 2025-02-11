{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.RIS.Presse
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Presse RIS format parser for Europresse Database.

-}


module Gargantext.Core.Text.Corpus.Parsers.RIS.Presse (presseEnrich) where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (length)
import Data.List (lookup)
import Data.Tuple.Extra (both)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Parsers.Date.Attoparsec qualified as Date
import Gargantext.Core.Text.Corpus.Parsers.RIS (onField)
import Gargantext.Prelude hiding (takeWhile, take, length)



presseEnrich :: [(ByteString, ByteString)] -> [(ByteString, ByteString)]
presseEnrich = (onField "DA" parseDate)
             . (onField "LA" parseLang)
             . fixFields
             

parseDate :: ByteString -> [(ByteString, ByteString)]
parseDate str = either (const []) identity $ parseOnly (Date.parserWith "/")  str

parseLang :: ByteString -> [(ByteString, ByteString)]
parseLang "Français" = [(langField, cs $ (show FR :: Text))]
parseLang "English"  = [(langField, cs $ (show EN :: Text))]
parseLang x = [(langField, x)]

langField :: ByteString
langField = "language"


fixFields :: [(ByteString, ByteString)] -> [(ByteString, ByteString)]
fixFields ns = map (first fixFields'') ns
  where
    -- | Title is sometimes longer than abstract
    fixFields'' = case uncurry (>) <$> look'' of
      Just True -> fixFields' "abstract" "title"
      _         -> fixFields' "title"    "abstract"

    look'' :: Maybe (Int, Int)
    look'' = both length <$> look

    look :: Maybe (ByteString,ByteString)
    look = (,) <$> lookup "TI" ns <*> lookup "N2" ns


    fixFields' :: ByteString -> ByteString
                 -> ByteString -> ByteString
    fixFields' title abstract champs
                | champs == "AU" = "authors"
                | champs == "TI" = title
                | champs == "JF" = "source"
                | champs == "DI" = "doi"
                | champs == "UR" = "url"
                | champs == "N2" = abstract
                | otherwise  = champs
