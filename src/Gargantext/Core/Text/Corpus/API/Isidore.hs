{-|
Module      : Gargantext.Core.Text.Corpus.API.Isidore
Description : To query French Humanities publication database from its API
Copyright   : (c) CNRS, 2019-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.Core.Text.Corpus.API.Isidore (
  get

  -- * Internals (possibly unused?)
  , isidore2csvFile
  ) where

import Data.Text qualified as Text
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Parsers (cleanText)
import Gargantext.Core.Text.Corpus.Parsers.CSV (writeDocs2Csv)
import Gargantext.Core.Text.Corpus.Parsers.Date qualified as Date
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument(..) )
import Gargantext.Defaults qualified as Defaults
import Gargantext.Prelude hiding (get)
import Isidore qualified as Isidore
import Isidore.Client
import Servant.Client ( ClientError(DecodeFailure) )

-- | TODO work with the ServantErr
get :: Lang
    -> Maybe Isidore.Limit
    -> Maybe Isidore.TextQuery
    -> Maybe Isidore.AuthorQuery
    -> IO [HyperdataDocument]
get lang l q a = do
  let
    printErr (DecodeFailure e _) = panicTrace e
    printErr e                   = panicTrace (show e)

    toIsidoreDocs :: Reply -> [IsidoreDoc]
    toIsidoreDocs (ReplyOnly r) = [r]
    toIsidoreDocs (Replies  rs) = rs

  iDocs <- either printErr _content <$> Isidore.get l q a

  hDocs <- mapM (isidoreToDoc lang) (toIsidoreDocs iDocs)
  pure hDocs

isidore2csvFile :: FilePath -> Lang -> Maybe Isidore.Limit
    -> Maybe Isidore.TextQuery -> Maybe Isidore.AuthorQuery
    -> IO ()
isidore2csvFile fp lang li tq aq = do
  hdocs <- get lang li tq aq
  writeDocs2Csv fp hdocs

isidoreToDoc :: Lang -> IsidoreDoc -> IO HyperdataDocument
isidoreToDoc lang (IsidoreDoc t a d u s as) = do
  let
    author :: Author -> Text
    author (Author fn ln) = _name fn <> ", " <> _name ln
    author (Authors aus) = Text.intercalate ". " $ map author aus

    creator2text :: Creator -> Text
    creator2text (Creator au)   = author au
    creator2text (Creators aus') = Text.intercalate ". " $ map author aus'

    langText :: LangText -> Text
    langText (LangText _l t1) = t1
    langText (OnlyText t2   ) = t2
    langText (ArrayText ts  ) = Text.intercalate " " $ map langText ts

  let mDateS = maybe (Just $ Text.pack $ show Defaults.year) Just d
  let (utcTime, (pub_year, pub_month, pub_day)) = Date.mDateSplit mDateS

  pure HyperdataDocument
         { _hd_bdd = Just "Isidore"
         , _hd_doi = Nothing
         , _hd_url = u
         , _hd_page = Nothing
         , _hd_title = Just $ cleanText $ langText t
         , _hd_authors = creator2text <$> as
         , _hd_institutes = Nothing
         , _hd_source = Just $ maybe "Nothing" (identity . _sourceName) s
         , _hd_abstract = cleanText . langText <$> a
         , _hd_publication_date = fmap (Text.pack . show) utcTime
         , _hd_publication_year = pub_year
         , _hd_publication_month = pub_month
         , _hd_publication_day = pub_day
         , _hd_publication_hour = Nothing
         , _hd_publication_minute = Nothing
         , _hd_publication_second = Nothing
         , _hd_language_iso2 = Just . Text.pack . show $ lang
         }
