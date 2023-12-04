{-|
Module      : CleanCsvCorpus.hs
Description : Gargantext starter
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Given a Gargantext CSV File and its Query This script cleans and
compress the contexts around the main terms of the query.
-}


module CleanCsvCorpus  where

import Data.SearchEngine qualified as S
import Data.Set qualified as S
import Data.Text (pack)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Gargantext.Core.Text.Corpus.Parsers.CSV qualified as CSV
import Gargantext.Core.Text.Search
import Gargantext.Prelude
------------------------------------------------------------------------

type Query = [S.Term]

filterDocs :: [DocId] -> Vector CSV.CsvGargV3 -> Vector CSV.CsvGargV3
filterDocs docIds = V.filter (\doc -> S.member (CSV.d_docId doc) $ S.fromList docIds )


main :: IO ()
main = do
  let rPath = "/tmp/Gargantext_Corpus.csv"
  let wPath = "/tmp/Gargantext_Corpus_bis.csv"
  --let q = ["water", "scarcity", "morocco", "shortage","flood"]
  let q = ["gratuit", "gratuité", "culture", "culturel"]

  eDocs <- CSV.readCSVFile rPath
  case eDocs of
    Right (h, csvDocs) -> do
      putStrLn ("Number of documents before:" <> show (V.length csvDocs) :: Text)
      putStrLn ("Mean size of docs:" <> show ( CSV.docsSize csvDocs) :: Text)

      let docs   = CSV.toDocs csvDocs
      let engine = S.insertDocs docs initialDocSearchEngine
      let docIds = S.query engine (map pack q)
      let docs'  = CSV.fromDocs $ filterDocs docIds (V.fromList docs)

      putStrLn ("Number of documents after:" <> show (V.length docs') :: Text)
      putStrLn ("Mean size of docs:" <> show (CSV.docsSize docs') :: Text)

      CSV.writeFile wPath (h, docs')
    Left e -> panicTrace $ "Error: " <> e
