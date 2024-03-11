{-|
Module      : Gargantext.API.Ngrams.List
Description : Get Ngrams (lists)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

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
import Gargantext.API.Prelude (GargServer, GargM, serverError, HasServerError)
import Gargantext.API.Types
import Gargantext.Core.NodeStory
import Gargantext.Core.Types.Main (ListType(..))
import Gargantext.Database.Action.Flow (reIndexWith)
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
              :> ( "json"
                   :> Get '[JSON, HTML] (Headers '[Header "Content-Disposition" Text] NgramsList)
                 :<|>  "json.zip"
                   :> Get '[GUS.ZIP] (Headers '[Header "Content-Disposition" Text] NgramsListZIP)
                 :<|> "csv"
                   :> Get '[GUS.CSV] (Headers '[Header "Content-Disposition" Text] NgramsTableMap) )
getApi :: GargServer GETAPI
getApi listId = getJson listId
           :<|> getJsonZip listId
           :<|> getCsv listId

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

getJsonZip :: HasNodeStory env err m
           => ListId
           -> m (Headers '[Header "Content-Disposition" Text] NgramsListZIP)
getJsonZip lId = do
  lst <- getNgramsList lId
  let nlz = NgramsListZIP { _nlz_nl = lst, _nlz_list_id = lId}
  pure $ addHeader (concat [ "attachment; filename="
                           , nlzFileName nlz
                           , ".zip"
                           ]
                     ) nlz

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
    postAsyncJSON lId (_wjf_data f) jHandle

------------------------------------------------------------------------
postAsyncJSON :: (HasNodeStory env err m, MonadJobStatus m)
              => ListId
              -> NgramsList
              -> JobHandle m
              -> m ()
postAsyncJSON l ngramsList jobHandle = do

  markStarted 2 jobHandle

  setList

  markProgress 1 jobHandle

  corpus_node <- getNode l -- (Proxy :: Proxy HyperdataList)
  let corpus_id = fromMaybe (panicTrace "no parent_id") (_node_parent_id corpus_node)
  _ <- reIndexWith corpus_id l NgramsTerms (Set.fromList [MapTerm, CandidateTerm])

  markComplete jobHandle

  where
    setList :: HasNodeStory env err m => m ()
    setList = do
      -- TODO check with Version for optim
      mapM_ (\(nt, Versioned _v ns) -> setListNgrams l nt ns) $ toList ngramsList
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
    case ngramsListFromCSVData (_wtf_data f) of
      Left err         -> serverError $ err500 { errReasonPhrase = err }
      Right ngramsList -> postAsyncJSON lId ngramsList jHandle

-- | Tries converting a text file into an 'NgramList', so that we can reuse the
-- existing JSON endpoint for the CSV upload.
ngramsListFromCSVData :: Text -> Either Prelude.String NgramsList
ngramsListFromCSVData csvData = case decodeCsv of
  -- /NOTE/ The legacy CSV data only supports terms in imports and exports, so this is
  -- all we care about.
  Left err    -> Left $ "Invalid CSV found in ngramsListFromCSVData: " <> err
  Right terms -> pure $ Map.fromList [ (NgramsTerms, Versioned 0 $ mconcat . Vec.toList $ terms) ]
  where
    binaryData = BSL.fromStrict $ P.encodeUtf8 csvData

    decodeCsv :: Either Prelude.String (Vector NgramsTableMap)
    decodeCsv = Csv.decodeWithP csvToNgramsTableMap
                               (Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (P.ord '\t') })
                               Csv.HasHeader
                               binaryData

-- | Converts a plain CSV 'Record' into an NgramsTableMap
csvToNgramsTableMap :: Csv.Record -> Csv.Parser NgramsTableMap
csvToNgramsTableMap record = case Vec.toList record of
  (map P.decodeUtf8 -> [status, label, forms])
    -> pure $ conv status label forms
  _ -> Prelude.fail "csvToNgramsTableMap failed"

  where
    conv :: Text -> Text -> Text -> NgramsTableMap
    conv status label forms = Map.singleton (NgramsTerm label)
        $ NgramsRepoElement { _nre_size = 1
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

------------------------------------------------------------------------


-- | This is for debugging the CSV parser in the REPL
importCsvFile :: forall env err m. (HasNodeStory env err m, HasServerError err, MonadJobStatus m)
              => ListId -> P.FilePath -> m ()
importCsvFile lId fp = do
  contents <- liftBase $ P.readFile fp
  case ngramsListFromCSVData contents of
    Left err         -> serverError $ err500 { errReasonPhrase = err }
    Right ngramsList -> postAsyncJSON lId ngramsList (noJobHandle @m Proxy)

--
-- Utils
--

------------------------------------------------------------------------
toIndexedNgrams :: HashMap Text NgramsId -> Text -> Maybe (Indexed Int Ngrams)
toIndexedNgrams m t = Indexed <$> i <*> n
  where
    i = HashMap.lookup t m
    n = Just (text2ngrams t)
