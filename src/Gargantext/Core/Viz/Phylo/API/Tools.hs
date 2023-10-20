{-|
Module      : Gargantext.Core.Viz.Phylo.API
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE TypeApplications #-}

module Gargantext.Core.Viz.Phylo.API.Tools
  where

import Control.Lens hiding (Context)
import Data.Aeson (Value, decodeFileStrict, eitherDecode, encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.Map.Strict qualified as Map
import Data.Proxy
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Time.Calendar (fromGregorian, diffGregorianDurationClip, cdMonths, diffDays, showGregorian)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import Gargantext.API.Ngrams.Prelude (getTermList)
import Gargantext.API.Ngrams.Types (NgramsTerm(..))
import Gargantext.Core (withDefaultLanguage, Lang)
import Gargantext.Core.NodeStory (HasNodeStory)
import Gargantext.Core.Text.Terms.WithList (Patterns, buildPatterns, termsInText)
import Gargantext.Core.Types (Context, nodeId2ContextId)
import Gargantext.Core.Types.Main (ListType(MapTerm))
import Gargantext.Core.Viz.Phylo (TimeUnit(..), Date, Document(..), PhyloConfig(..), Phylo)
import Gargantext.Core.Viz.Phylo.PhyloExport (toPhyloExport, dotToFile)
import Gargantext.Core.Viz.Phylo.PhyloMaker  (toPhylo, toPhyloWithoutLink)
import Gargantext.Core.Viz.Phylo.PhyloTools  ({-printIOMsg, printIOComment,-} setConfig)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataPhylo(..), HyperdataCorpus(..))
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Database.Admin.Types.Node (CorpusId, ContextId, PhyloId)
import Gargantext.Database.Prelude (DBCmd)
import Gargantext.Database.Query.Table.Node (defaultList, getNodeWith)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.NodeContext (selectDocNodes)
import Gargantext.Database.Schema.Context
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Database.Schema.Node
import Gargantext.Prelude hiding (to)
import Prelude qualified
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import System.Process qualified as Shell

--------------------------------------------------------------------
getPhyloData :: HasNodeError err
             => PhyloId -> DBCmd err (Maybe Phylo)
getPhyloData phyloId = do
  nodePhylo <- getNodeWith phyloId (Proxy :: Proxy HyperdataPhylo)
  pure $ _hp_data $ _node_hyperdata nodePhylo

putPhylo :: PhyloId -> DBCmd err Phylo
putPhylo = undefined

savePhylo :: PhyloId -> DBCmd err ()
savePhylo = undefined

--------------------------------------------------------------------
phylo2dot2json :: Phylo -> IO Value
phylo2dot2json phylo = do
  withTempDirectory "/tmp" "phylo" $ \dirPath -> do
    let fileFrom = dirPath </> "phyloFrom.dot"
        fileDot  = dirPath </> "phylo.dot"
        fileToJson = dirPath </> "output.json"

    dotToFile fileFrom (toPhyloExport phylo)

    -- parsing a file can be done with:
    -- runParser' (Data.GraphViz.Parsing.parse :: Parse (Data.GraphViz.DotGraph Text)) $ TL.fromStrict f
    Shell.callProcess "dot" ["-Tdot", "-o", fileDot, fileFrom]
    Shell.callProcess "dot" ["-Txdot_json", "-o", fileToJson, fileDot]

    maybeValue <- decodeFileStrict fileToJson
    -- print maybeValue

    case maybeValue of
      Nothing -> panic "[G.C.V.Phylo.API.phylo2dot2json] Error no file"
      Just v  -> pure v


flowPhyloAPI :: (HasNodeStory env err m, HasNodeError err)
             => PhyloConfig -> CorpusId -> m Phylo
flowPhyloAPI config cId = do
  corpus <- corpusIdtoDocuments (timeUnit config) cId
  let phyloWithCliques = toPhyloWithoutLink corpus config
  -- writePhylo phyloWithCliquesFile phyloWithCliques
  printDebug "PhyloConfig old: " config

  pure $ toPhylo $ setConfig config phyloWithCliques

--------------------------------------------------------------------
corpusIdtoDocuments :: (HasNodeStory env err m, HasNodeError err)
                    => TimeUnit -> CorpusId -> m [Document]
corpusIdtoDocuments timeUnit corpusId = do
  docs <- selectDocNodes corpusId
  lId  <- defaultList corpusId
  termList <- getTermList lId MapTerm NgramsTerms
  corpus_node <- getNodeWith corpusId (Proxy @ HyperdataCorpus)
  let corpusLang = view (node_hyperdata . to _hc_lang) corpus_node

  let patterns = case termList of
        Nothing        -> panic "[G.C.V.Phylo.API] no termList found"
        Just termList' -> buildPatterns termList'
  pure $ map (toPhyloDocs (withDefaultLanguage corpusLang) patterns timeUnit) (map _context_hyperdata docs)

termsInText' :: Lang -> Patterns -> Text -> [Text]
termsInText' lang p t = (map fst) $ termsInText lang p t

toPhyloDocs :: Lang -> Patterns -> TimeUnit -> HyperdataDocument -> Document
toPhyloDocs lang patterns time d =
  let title = fromMaybe "" (_hd_title d)
      abstr = fromMaybe "" (_hd_abstract d)
                  in Document (toPhyloDate
                                      (fromIntegral $ fromMaybe 1 $ _hd_publication_year d)
                                      (fromMaybe 1 $ _hd_publication_month d)
                                      (fromMaybe 1 $ _hd_publication_day d) time)
                                    (toPhyloDate'
                                      (fromIntegral $ fromMaybe 1 $ _hd_publication_year d)
                                      (fromMaybe 1 $ _hd_publication_month d)
                                      (fromMaybe 1 $ _hd_publication_day d) time)
                                    (termsInText' lang patterns $ title <> " " <> abstr) Nothing [] time



context2phyloDocument :: TimeUnit
                      -> Context HyperdataDocument
                      -> (Map ContextId (Set NgramsTerm), Map ContextId (Set NgramsTerm))
                      -> Maybe Document
context2phyloDocument timeUnit context (ngs_terms, ngs_sources) = do
  let contextId = _context_id context
  (date, date') <- context2date context timeUnit

  let
    toText x = Set.toList $ Set.map unNgramsTerm x

    text'    = maybe [] toText $ Map.lookup (nodeId2ContextId contextId) ngs_terms
    sources' = maybe [] toText $ Map.lookup (nodeId2ContextId contextId) ngs_sources

  pure $ Document date date' text' Nothing sources' (Year 3 1 5)


-- TODO better default date and log the errors to improve data quality
context2date :: Context HyperdataDocument -> TimeUnit -> Maybe (Date, Text)
context2date context timeUnit = do
  let hyperdata =  _context_hyperdata context
  let
    year  = fromMaybe 1 $ _hd_publication_year  hyperdata
    month = fromMaybe 1 $ _hd_publication_month hyperdata
    day   = fromMaybe 1 $ _hd_publication_day   hyperdata
  pure (toPhyloDate year month day timeUnit, toPhyloDate' year month day timeUnit)


---------------
-- | Dates | --
---------------
toMonths :: Integer -> Int -> Int -> Date
toMonths y m d = fromIntegral $ cdMonths
               $ diffGregorianDurationClip (fromGregorian y    m d)
                                           (fromGregorian 0000 0 0)

toDays :: Integer -> Int -> Int -> Date
toDays y m d = fromIntegral
             $ diffDays (fromGregorian y m d) (fromGregorian 0000 0 0)

toPhyloDate :: Int -> Int -> Int -> TimeUnit -> Date
toPhyloDate y m d tu = case tu of
  Year  {} -> y
  Month {} -> toMonths (Prelude.toInteger y) m d
  Week  {} -> div (toDays (Prelude.toInteger y) m d) 7
  Day   {} -> toDays (Prelude.toInteger y) m d
  _        -> panic "[G.C.V.Phylo.API] toPhyloDate"

toPhyloDate' :: Int -> Int -> Int -> TimeUnit -> Text
toPhyloDate' y _m _d (Epoch {}) = pack $ show $ posixSecondsToUTCTime $ fromIntegral y
toPhyloDate' y  m  d _          = pack $ showGregorian $ fromGregorian (toInteger y) m d

-- Utils

writePhylo :: [Char] -> Phylo -> IO ()
writePhylo path phylo = Lazy.writeFile path $ encode phylo


readPhylo :: [Char] -> IO Phylo
readPhylo path = do
  phyloJson <- (eitherDecode <$> readJson path) :: IO (Either Prelude.String Phylo)
  case phyloJson of
    Left err -> do
      putStrLn err
      undefined
    Right phylo -> pure phylo


-- | To read and decode a Json file
readJson :: FilePath -> IO Lazy.ByteString
readJson = Lazy.readFile
