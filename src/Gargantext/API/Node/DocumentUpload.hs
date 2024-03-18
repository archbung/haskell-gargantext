{-|
Module      : Gargantext.API.Node.DocumentUpload
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Gargantext.API.Node.DocumentUpload where

import Control.Lens (makeLenses, view)
import Data.Aeson
import Data.Swagger (ToSchema)
import Data.Text qualified as T
import Gargantext.API.Admin.EnvTypes (GargJob(..), Env)
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Errors.Types ( BackendInternalError )
import Gargantext.API.Prelude ( GargM )
import Gargantext.Core (Lang(..))
import Gargantext.Core.NLP (nlpServerGet)
import Gargantext.Core.Text.Corpus.Parsers.Date (mDateSplit)
import Gargantext.Core.Text.Terms (TermType(..))
import Gargantext.Core.Utils.Prefix (unCapitalize, dropPrefix)
import Gargantext.Database.Action.Flow (addDocumentsToHyperCorpus)
import Gargantext.Database.Action.Flow.Types ( FlowCmdM )
import Gargantext.Database.Admin.Types.Hyperdata.Corpus ( HyperdataCorpus )
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Database.Admin.Types.Node ( DocId, NodeId, NodeType(NodeCorpus) )
import Gargantext.Database.Query.Table.Node (getClosestParentIdByType')
import Gargantext.Prelude
import Gargantext.Utils.Jobs (serveJobsAPI, MonadJobStatus(..))
import Servant ( JSON, Summary, type (:>), HasServer(ServerT) )


data DocumentUpload = DocumentUpload
  { _du_abstract :: T.Text
  , _du_authors  :: T.Text
  , _du_sources  :: T.Text
  , _du_title    :: T.Text
  , _du_date     :: T.Text
  , _du_language :: T.Text
  }
  deriving (Generic)

$(makeLenses ''DocumentUpload)

instance ToSchema DocumentUpload
instance FromJSON DocumentUpload
  where
    parseJSON = genericParseJSON
            ( defaultOptions { sumEncoding = ObjectWithSingleField
                            , fieldLabelModifier = unCapitalize . dropPrefix "_du_"
                            , omitNothingFields = True
                            }
            )
instance ToJSON DocumentUpload
  where
    toJSON = genericToJSON
           ( defaultOptions { sumEncoding = ObjectWithSingleField
                            , fieldLabelModifier = unCapitalize . dropPrefix "_du_"
                            , omitNothingFields = True
                            }
           )

type API = Summary " Document upload"
           :> "document"
           :> "upload"
           :> "async"
           :> AsyncJobs JobLog '[JSON] DocumentUpload JobLog

api :: NodeId -> ServerT API (GargM Env BackendInternalError)
api nId =
  serveJobsAPI UploadDocumentJob $ \jHandle q -> do
      documentUploadAsync nId q jHandle

documentUploadAsync :: (FlowCmdM env err m, MonadJobStatus m)
               => NodeId
               -> DocumentUpload
               -> JobHandle m
               -> m ()
documentUploadAsync nId doc jobHandle = do
  markStarted 1 jobHandle
  _docIds <- documentUpload nId doc
  -- printDebug "documentUploadAsync" docIds
  markComplete jobHandle

documentUpload :: (FlowCmdM env err m)
               => NodeId
               -> DocumentUpload
               -> m [DocId]
documentUpload nId doc = do
  mcId <- getClosestParentIdByType' nId NodeCorpus
  let cId = case mcId of
        Just c  -> c
        Nothing -> panicTrace $ T.pack $ "[G.A.N.DU] Node has no corpus parent: " <> show nId

  let mDateS = Just $ view du_date doc
  let (theFullDate, (year, month, day)) = mDateSplit mDateS

  let hd = HyperdataDocument { _hd_bdd = Nothing
                             , _hd_doi = Nothing
                             , _hd_url = Nothing
                             , _hd_page = Nothing
                             , _hd_title = Just $ if view du_title doc == "" then T.take 50 (view du_abstract doc) else view du_title doc
                             , _hd_authors = Just $ view du_authors doc
                             , _hd_institutes = Nothing
                             , _hd_source             = Just $ view du_sources doc
                             , _hd_abstract           = Just $ view du_abstract doc
                             , _hd_publication_date   = fmap (T.pack . show) theFullDate
                             , _hd_publication_year   = year
                             , _hd_publication_month  = month
                             , _hd_publication_day    = day
                             , _hd_publication_hour   = Nothing
                             , _hd_publication_minute = Nothing
                             , _hd_publication_second = Nothing
                             , _hd_language_iso2 = Just $ view du_language doc }

  let lang = EN
  ncs <- view $ nlpServerGet lang
  addDocumentsToHyperCorpus ncs (Nothing :: Maybe HyperdataCorpus) (Multi lang) cId [hd]
