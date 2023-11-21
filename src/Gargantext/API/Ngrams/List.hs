{-|
Module      : Gargantext.API.Ngrams.List
Description : Get Ngrams (lists)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.API.Ngrams.List
  where

import Data.ByteString.Lazy qualified as BSL
import Data.Csv qualified as Csv
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict (toList)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (concat, pack, splitOn)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Gargantext.API.Admin.EnvTypes (Env, GargJob(..))
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Errors.Types
import Gargantext.API.Ngrams (setListNgrams)
import Gargantext.API.Ngrams.List.Types
import Gargantext.API.Ngrams.Prelude (getNgramsList)
import Gargantext.API.Ngrams.Types
import Gargantext.API.Prelude (GargServer, GargM)
import Gargantext.API.Types
import Gargantext.Core.NodeStory
import Gargantext.Core.Types.Main (ListType(..))
import Gargantext.Database.Action.Flow (reIndexWith)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Ngrams qualified as TableNgrams
import Gargantext.Database.Query.Table.Node (getNode)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.Node (_node_parent_id)
import Gargantext.Database.Types (Indexed(..))
import Gargantext.Prelude hiding (concat, toList)
import Gargantext.Utils.Jobs (serveJobsAPI, MonadJobStatus(..))
import Gargantext.Utils.Servant qualified as GUS
import Prelude qualified
import Protolude qualified as P
import Servant

------------------------------------------------------------------------
type GETAPI = Summary "Get List"
            :> "lists"
              :> Capture "listId" ListId
              :> "json"
              :> Get '[JSON, HTML] (Headers '[Header "Content-Disposition" Text] NgramsList)
            :<|> "lists"
              :> Capture "listId" ListId
              :> "csv"
              :> Get '[GUS.CSV] (Headers '[Header "Content-Disposition" Text] NgramsTableMap)
getApi :: GargServer GETAPI
getApi = getJson :<|> getCsv

--
-- JSON API
--

----------------------
type JSONAPI = Summary "Update List"
          :> "lists"
          :> Capture "listId" ListId
          :> "add"
          :> "form"
          :> "async"
          :> AsyncJobs JobLog '[FormUrlEncoded] WithJsonFile JobLog

jsonApi :: ServerT JSONAPI (GargM Env BackendInternalError)
jsonApi = jsonPostAsync

------------------------------------------------------------------------
getJson :: HasNodeStory env err m
        => ListId
        -> m (Headers '[Header "Content-Disposition" Text] NgramsList)
getJson lId = do
  lst <- getNgramsList lId
  pure $ addHeader (concat [ "attachment; filename=GarganText_NgramsList-"
                             , pack $ show (_NodeId lId)
                             , ".json"
                             ]
                     ) lst

getCsv :: HasNodeStory env err m
       => ListId
       -> m (Headers '[Header "Content-Disposition" Text] NgramsTableMap)
getCsv lId = do
  lst <- getNgramsList lId
  pure $ case Map.lookup TableNgrams.NgramsTerms lst of
    Nothing -> noHeader Map.empty
    Just (Versioned { _v_data }) ->
      addHeader (concat [ "attachment; filename=GarganText_NgramsList-"
                        , pack $ show (_NodeId lId)
                        , ".csv"
                        ]
                ) _v_data

------------------------------------------------------------------------
jsonPostAsync :: ServerT JSONAPI (GargM Env BackendInternalError)
jsonPostAsync lId =
  serveJobsAPI UpdateNgramsListJobJSON $ \jHandle f ->
    postAsyncJSON lId f jHandle

------------------------------------------------------------------------
postAsyncJSON :: (FlowCmdM env err m, MonadJobStatus m)
              => ListId
              -> WithJsonFile
              -> JobHandle m
              -> m ()
postAsyncJSON l (WithJsonFile m _) jobHandle = do

  markStarted 2 jobHandle
  -- printDebug "New list as file" l
  setList
  -- printDebug "Done" r

  markProgress 1 jobHandle

  corpus_node <- getNode l -- (Proxy :: Proxy HyperdataList)
  let corpus_id = fromMaybe (panic "no parent_id") (_node_parent_id corpus_node)
  _ <- reIndexWith corpus_id l NgramsTerms (Set.fromList [MapTerm, CandidateTerm])

  markComplete jobHandle

  where
    setList :: HasNodeStory env err m => m ()
    setList = do
      -- TODO check with Version for optim
      mapM_ (\(nt, Versioned _v ns) -> (setListNgrams l nt ns)) $ toList m
      -- TODO reindex


--
-- CSV API
--

----------------------
type CSVAPI = Summary "Update List (legacy v3 CSV)"
            :> "lists"
            :> Capture "listId" ListId
            :> "csv"
            :> "add"
            :> "form"
            :> "async"
            :> AsyncJobs JobLog '[FormUrlEncoded] WithTextFile JobLog

csvApi :: ServerT CSVAPI (GargM Env BackendInternalError)
csvApi = csvPostAsync

------------------------------------------------------------------------
csvPostAsync :: ServerT CSVAPI (GargM Env BackendInternalError)
csvPostAsync lId =
  serveJobsAPI UpdateNgramsListJobCSV $ \jHandle f -> do
    postAsyncCSV lId f jHandle

postAsyncCSV :: (FlowCmdM env err m, MonadJobStatus m)
             => ListId
             -> WithTextFile
             -> JobHandle m
             -> m ()
postAsyncCSV l (WithTextFile _filetype csvData _name) jHandle = do
  markStarted 2 jHandle

  let eLst = readCsvText csvData
  case eLst of
    Left err -> markFailed (Just err) jHandle
    Right lst -> do
      let p = parseCsvData lst
      _ <- setListNgrams l NgramsTerms p
      markProgress 1 jHandle
      corpus_node <- getNode l -- (Proxy :: Proxy HyperdataList)
      let corpus_id = fromMaybe (panic "") (_node_parent_id corpus_node)
      _ <- reIndexWith corpus_id l NgramsTerms (Set.fromList [MapTerm, CandidateTerm])
      markComplete jHandle

------------------------------------------------------------------------
readCsvText :: Text -> Either Text [(Text, Text, Text)]
readCsvText t = case eDec of
  Left err -> Left $ pack err
  Right dec -> Right $ Vec.toList dec
  where
    lt = BSL.fromStrict $ P.encodeUtf8 t
    eDec = Csv.decodeWith
             (Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (P.ord '\t') })
             Csv.HasHeader lt :: Either Prelude.String (Vector (Text, Text, Text))

parseCsvData :: [(Text, Text, Text)] -> Map NgramsTerm NgramsRepoElement
parseCsvData lst = Map.fromList $ conv <$> lst
  where
    conv (status, label, forms) =
        (NgramsTerm label, NgramsRepoElement { _nre_size = 1
                                             , _nre_list = case status == "map" of
                                                             True  -> MapTerm
                                                             False -> case status == "main" of
                                                                True  -> CandidateTerm
                                                                False -> StopTerm
                                             , _nre_root = Nothing
                                             , _nre_parent = Nothing
                                             , _nre_children = MSet
                                                             $ Map.fromList
                                                             $ map (\form -> (NgramsTerm form, ()))
                                                             $ filter (\w -> w /= "" && w /= label)
                                                             $ splitOn "|&|" forms
                                             }
         )

------------------------------------------------------------------------


-- | This is for debugging the CSV parser in the REPL
--importCsvFile :: (HasNodeStory env err m)
--              => ListId -> P.FilePath -> m ()
--importCsvFile lId fp = do
--  contents <- liftBase $ P.readFile fp
--  postAsyncCSV lId (WithTextFile mempty contents mempty) noJobHandle

--
-- Utils
--

------------------------------------------------------------------------
toIndexedNgrams :: HashMap Text NgramsId -> Text -> Maybe (Indexed Int Ngrams)
toIndexedNgrams m t = Indexed <$> i <*> n
  where
    i = HashMap.lookup t m
    n = Just (text2ngrams t)

