{-|
Module      : Main.hs
Description : Gargantext starter
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main specifications to index a corpus with a term list

 -}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE Strict             #-}

module Main where

import Control.Concurrent.Async as CCA (mapConcurrently)
import Control.Monad.IO.Class
import Data.Aeson
import Data.List.Split (chunksOf)
import Data.Map.Strict qualified as DM
import Data.Text (pack)
import Data.Text qualified as DT
import Data.Text.Lazy qualified as DTL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Tuple.Extra (both)
import Data.Vector qualified as DV
import GHC.Generics
import Gargantext.Core.Text.Context
import Gargantext.Core.Text.Corpus.Parsers.CSV (readCSVFile, csv_title, csv_abstract, csv_publication_year, fromMIntOrDec, defaultYear)
import Gargantext.Core.Text.List.Formats.CSV (csvMapTermList)
import Gargantext.Core.Text.Metrics.Count (coocOnContexts, Coocs)
import Gargantext.Core.Text.Terms.WithList
import Gargantext.Prelude hiding (show)
import Protolude
import System.IO (hFlush)

------------------------------------------------------------------------
-- OUTPUT format

data CoocByYear = CoocByYear { year         :: Int
                             , nbContexts   :: NbContexts
                             , coocurrences :: Map (Text, Text) Coocs
                             } deriving (Show, Generic)

data CoocByYears = CoocByYears { years :: [CoocByYear] }
  deriving (Show, Generic)

type NbContexts = Int

instance ToJSON CoocByYear
instance ToJSON CoocByYears
------------------------------------------------------------------------

filterTermsAndCooc
  :: Patterns
     -> (Int, [Text])
     -> IO CoocByYear -- (Int, (Map (Text, Text) Coocs))
filterTermsAndCooc patterns (year, ts) = do
  logWork "start"
  r <- coocOnContexts identity <$> mapM (\x -> {-log "work" >>-} terms' patterns x) ts
  logWork "stop"
  pure $ CoocByYear year (length ts) (DM.mapKeys (both DT.unwords) r)
  where

    logWork m = do
      tid    <- myThreadId
      (p, _) <- threadCapability tid
      putText . unwords $
        ["filterTermsAndCooc:", m, show year, "on proc", show p]

main :: IO ()
main = do
  [corpusFile, termListFile, outputFile] <- getArgs

  --corpus :: IO (DM.IntMap [[Text]])
  eCorpusFile <- readCSVFile corpusFile
  case eCorpusFile of
    Right cf -> do
      let corpus = DM.fromListWith (<>)
                   . DV.toList
                   . DV.map (\n -> (fromMIntOrDec defaultYear $ csv_publication_year n, [(csv_title n) <> " " <> (csv_abstract n)]))
                   . snd $ cf

      -- termListMap :: [Text]
      termList <- csvMapTermList termListFile

      putText $ show $ length termList

      let patterns = buildPatterns termList

      -- r <- mapConcurrentlyChunked (filterTermsAndCooc patterns) (DM.toList corpus)
      r <-  mapConcurrently (filterTermsAndCooc patterns) (DM.toList corpus)
      writeFile outputFile $ DTL.toStrict $ TLE.decodeUtf8 $ encode (CoocByYears r)
    Left e -> panicTrace $ "Error: " <> e



------------------------------------------------------------------------
-- | Tools
mapMP :: MonadIO m => (a -> m b) -> [a] -> m [b]
mapMP f xs = do
    bs <- zipWithM g (cycle "-\\|/") xs
    liftIO $ hPutStr stderr ("\rDone\n" :: Text)
    pure bs
  where
    g c x = do
      liftIO $ hPutStr stderr ['\r',c]
      liftIO $ hFlush  stderr
      f x

-- | Optimi that need further developments (not used yet)
mapConcurrentlyChunked :: (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyChunked f ts = do
  caps <- getNumCapabilities
  let n = 1 `max` (length ts `div` caps)
  concat <$> mapConcurrently (mapM f) (chunksOf n ts)


--terms' :: Patterns -> Text -> Corpus [[Text]]
terms' :: Applicative f => Patterns -> Text -> f [[Text]]
terms' pats txt = pure $ concat $ extractTermsWithList pats txt


-- | TODO Minimal Example
--testCooc = do
--  let patterns = buildPatterns testTermList
--  mapM (\x -> {-log "work" >>-} terms' patterns x) $ catMaybes $ map (head . snd) testCorpus
--  --mapConcurrently (filterTermsAndCooc patterns) testCorpus


testCorpus :: [(Int, [Text])]
testCorpus = [ (1998, [pack "The beees"])
             , (1999, [ pack "The bees and the flowers"
                      --, pack "The bees and the flowers"
                      ])
             ]

testTermList :: TermList
testTermList = [ ([pack "bee"], [[pack "bees"]])
               , ([pack "flower"], [[pack "flowers"]])
               ]
