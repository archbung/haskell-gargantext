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
import Gargantext.API.Ngrams (setListNgrams)
import Gargantext.API.Ngrams.List.Types
import Gargantext.API.Ngrams.Prelude (getNgramsList)
import Gargantext.API.Ngrams.Types
import Gargantext.API.Prelude (GargServer, GargM, GargError)
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

----------------------
type JSONAPI = Summary "Update List"
          :> "lists"
            :> Capture "listId" ListId
          :> "add"
          :> "form"
          :> "async"
            :> AsyncJobs JobLog '[FormUrlEncoded] WithJsonFile JobLog

jsonApi :: ServerT JSONAPI (GargM Env GargError)
jsonApi = jsonPostAsync

----------------------
type CSVAPI = Summary "Update List (legacy v3 CSV)"
          :> "lists"
            :> Capture "listId" ListId
          :> "csv"
          :> "add"
          :> "form"
          :> "async"
            :> AsyncJobs JobLog '[FormUrlEncoded] WithTextFile JobLog

csvApi :: ServerT CSVAPI (GargM Env GargError)
csvApi = csvPostAsync

------------------------------------------------------------------------
getJson :: HasNodeStory env err m =>
       ListId -> m (Headers '[Header "Content-Disposition" Text] NgramsList)
getJson lId = do
  lst <- getNgramsList lId
  let (NodeId id') = lId
  pure $ addHeader (concat [ "attachment; filename=GarganText_NgramsList-"
                             , pack $ show id'
                             , ".json"
                             ]
                     ) lst

getCsv :: HasNodeStory env err m =>
       ListId -> m (Headers '[Header "Content-Disposition" Text] NgramsTableMap)
getCsv lId = do
  lst <- getNgramsList lId
  let (NodeId id') = lId
  pure $ case Map.lookup TableNgrams.NgramsTerms lst of
    Nothing -> noHeader Map.empty
    Just (Versioned { _v_data }) ->
      addHeader (concat [ "attachment; filename=GarganText_NgramsList-"
                        , pack $ show id'
                        , ".csv"
                        ]
                ) _v_data

------------------------------------------------------------------------
-- TODO : purge list
-- TODO talk
setList :: HasNodeStory env err m
        => ListId
        -> NgramsList
        -> m Bool
setList l m  = do
  -- TODO check with Version for optim
  -- printDebug "New list as file" l
  _ <- mapM (\(nt, Versioned _v ns) -> setListNgrams l nt ns) $ toList m
  -- TODO reindex
  pure True

------------------------------------------------------------------------

toIndexedNgrams :: HashMap Text NgramsId -> Text -> Maybe (Indexed Int Ngrams)
toIndexedNgrams m t = Indexed <$> i <*> n
  where
    i = HashMap.lookup t m
    n = Just (text2ngrams t)

------------------------------------------------------------------------
jsonPostAsync :: ServerT JSONAPI (GargM Env GargError)
jsonPostAsync lId =
  serveJobsAPI UpdateNgramsListJobJSON $ \jHandle f ->
    postAsync' lId f jHandle

postAsync' :: (FlowCmdM env err m, MonadJobStatus m)
          => ListId
          -> WithJsonFile
          -> JobHandle m
          -> m ()
postAsync' l (WithJsonFile m _) jobHandle = do

  markStarted 2 jobHandle
  -- printDebug "New list as file" l
  _ <- setList l m
  -- printDebug "Done" r

  markProgress 1 jobHandle

  corpus_node <- getNode l -- (Proxy :: Proxy HyperdataList)
  let corpus_id = fromMaybe (panic "") (_node_parent_id corpus_node)
  _ <- reIndexWith corpus_id l NgramsTerms (Set.fromList [MapTerm, CandidateTerm])

  markComplete jobHandle

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

csvPost :: HasNodeStory env err m
        => ListId
        -> Text
        -> m (Either Text ())
csvPost l m  = do
  -- printDebug "[csvPost] l" l
  -- printDebug "[csvPost] m" m
  -- status label forms
  let eLst = readCsvText m
  case eLst of
    Left err -> pure $ Left err
    Right lst -> do
      let p = parseCsvData lst
      --printDebug "[csvPost] lst" lst
      -- printDebug "[csvPost] p" p
      _ <- setListNgrams l NgramsTerms p
      -- printDebug "ReIndexing List" l
      corpus_node <- getNode l -- (Proxy :: Proxy HyperdataList)
      let corpus_id = fromMaybe (panic "") (_node_parent_id corpus_node)
      _ <- reIndexWith corpus_id l NgramsTerms (Set.fromList [MapTerm, CandidateTerm])

      pure $ Right ()

------------------------------------------------------------------------
csvPostAsync :: ServerT CSVAPI (GargM Env GargError)
csvPostAsync lId =
  serveJobsAPI UpdateNgramsListJobCSV $ \jHandle f -> do
      markStarted 1 jHandle
      ePost <- csvPost lId (_wtf_data f)
      case ePost of
        Left err -> markFailed (Just err) jHandle
        Right () -> markComplete jHandle

      getLatestJobStatus jHandle >>= printDebug "[csvPostAsync] job ended with joblog: "

------------------------------------------------------------------------


-- | This is for debugging the CSV parser in the REPL
importCsvFile :: (HasNodeStory env err m)
              => ListId -> P.FilePath -> m (Either Text ())
importCsvFile lId fp = do
  contents <- liftBase $ P.readFile fp
  csvPost lId contents
