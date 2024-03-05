{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.JSON.Istex
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Parser for a single file in istex zip. See
https://gitlab.iscpif.fr/gargantext/purescript-gargantext/issues/603

-}

{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.Core.Text.Corpus.Parsers.JSON.Istex where

import Data.Text qualified as T
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Parsers.Date qualified as Date
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Defaults qualified as Defaults
import Gargantext.Prelude hiding (length)
import ISTEX.Client qualified as ISTEX


-- | TODO remove dateSplit here
-- TODO current year as default
toDoc :: Lang -> ISTEX.Document -> IO HyperdataDocument
toDoc la (ISTEX.Document i t a ab d s) = do
  --printDebug "ISTEX date" d
  let mDateS = maybe (Just $ T.pack $ show Defaults.year) (Just . T.pack . show) d
  let (utctime, (pub_year, pub_month, pub_day)) = Date.mDateSplit mDateS
  --printDebug "toDoc Istex" (utctime, (pub_year, pub_month, pub_day))
  pure $ HyperdataDocument { _hd_bdd       = Just "Istex"
                           , _hd_doi       = Just i
                           , _hd_url       = Nothing
                           , _hd_page      = Nothing
                           , _hd_title     = t
                           , _hd_authors = Just $ foldl' (\x y -> if x == "" then y else x <> ", " <> y) "" (map ISTEX._author_name a)
                           , _hd_institutes = Just $ foldl' (\x y -> if x == "" then y else x <> ", " <> y) "" (concatMap ISTEX._author_affiliations a)
                           , _hd_source = Just $ foldl' (\x y -> if x == "" then y else x <> ", " <> y) "" (ISTEX._source_title s)
                           , _hd_abstract = ab
                           , _hd_publication_date = fmap (T.pack . show) utctime
                           , _hd_publication_year = pub_year
                           , _hd_publication_month = pub_month
                           , _hd_publication_day = pub_day
                           , _hd_publication_hour = Nothing
                           , _hd_publication_minute = Nothing
                           , _hd_publication_second = Nothing
                           , _hd_language_iso2 = Just $ (T.pack . show) la
                           }

