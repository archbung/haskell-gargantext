{-|
Module      : Main.hs
Description : Gargantext Import Corpus
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Import a corpus binary.

 -}

{-# LANGUAGE Strict            #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Gargantext.API.Dev (withDevEnv)
import Gargantext.API.Node () -- instances only
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig(..), readConfig)
import Prelude (getLine)
import System.Environment (getArgs)
import qualified Data.List as List (cycle, concat, take, unlines)

main :: IO ()
main = do

  let ___ = putStrLn
          $ List.concat
          $ List.take 72
          $ List.cycle ["_"]

  ___
  putStrLn "GarganText upgrade to version 0.0.6.9.9.4.4"
  ___

  params@[iniPath] <- getArgs
  _ <- if length params /= 1
         then panic "Usage: ./gargantext-upgrade gargantext.ini"
         else pure ()

  putStrLn $ List.unlines
           [ "Your Database defined in gargantext.ini will be upgraded."
           , "We stronlgy recommend you to make a backup using pg_dump."
           , ""
           , "Press ENTER if you want to continue, CTRL+C if you want to stop."
           ]

  _ok  <- getLine

  cfg       <- readConfig         iniPath
  let _secret = _gc_secretkey cfg

  withDevEnv iniPath $ \env -> do
    -- _ <- runCmdDev env addIndex
    -- _ <- runCmdDev env refreshIndex


    ___
    putStrLn "Uprade done with success !"
    ___
    pure ()

-- refreshIndex :: Cmd'' DevEnv IOException ()
-- refreshIndex = do
--   _ <- execPGSQuery [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY context_node_ngrams_view; |] ()
--   pure ()

-- addIndex :: Cmd'' DevEnv IOException Int64
-- addIndex = do
--   execPGSQuery query ()
--     where
--       query = [sql|
--         CREATE MATERIALIZED VIEW IF NOT EXISTS context_node_ngrams_view AS
--           SELECT DISTINCT context_node_ngrams.context_id, ngrams_id, nodes_contexts.node_id
--           FROM nodes_contexts
--           JOIN context_node_ngrams
--           ON context_node_ngrams.context_id = nodes_contexts.context_id;

--         CREATE INDEX IF NOT EXISTS context_node_ngrams_context_id_ngrams_id_idx
--           ON context_node_ngrams(context_id, ngrams_id);

--         CREATE INDEX IF NOT EXISTS context_node_ngrams_view_context_id_idx
--           ON context_node_ngrams_view(context_id);
--         CREATE INDEX IF NOT EXISTS context_node_ngrams_view_ngrams_id_idx
--           ON context_node_ngrams_view(ngrams_id);
--         CREATE INDEX IF NOT EXISTS context_node_ngrams_view_node_id_idx
--           ON context_node_ngrams_view(node_id);
--         CREATE UNIQUE INDEX IF NOT EXISTS context_node_ngrams_view_context_ngrams_node_uniq_idx
--           ON context_node_ngrams_view (context_id, ngrams_id, node_id);

--         CREATE INDEX IF NOT EXISTS node_stories_ngrams_id_idx
--           ON node_stories(ngrams_id);
--   |]
