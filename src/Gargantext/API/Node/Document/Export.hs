{-|
Module      : Gargantext.API.Node.Document.Export
Description : Document export
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.API.Node.Document.Export
  where

-- import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Control.Lens (view)
import Data.Csv (encodeDefaultOrderedByName)
import Data.Version (showVersion)
import Gargantext.API.Node.Document.Export.Types
import Gargantext.API.Prelude (GargNoServer, GargServer)
import Gargantext.Core (toDBid)
import Gargantext.Core.Types
import Gargantext.Database.Query.Facet (runViewDocuments, Facet(..))
import Gargantext.Database.Query.Table.Node (getClosestParentIdByType)
import Gargantext.Database.Query.Table.Node.User
import Gargantext.Database.Schema.Node (NodePoly(..), node_user_id)
import Gargantext.Prelude
import Servant
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Paths_gargantext as PG -- cabal magic build module

api :: NodeId
    -- ^ The ID of the target user
    -> DocId
    -> GargServer API
api userNodeId dId = getDocumentsJSON userNodeId dId
                :<|> getDocumentsCSV userNodeId dId

--------------------------------------------------
-- | Hashes are ordered by Set
getDocumentsJSON :: NodeId
                 -- ^ The ID of the target user
                 -> DocId
                 -> GargNoServer (Headers '[Header "Content-Disposition" T.Text] DocumentExport)
getDocumentsJSON nodeUserId pId = do
  uId <- view node_user_id <$> getNodeUser nodeUserId
  mcId <- getClosestParentIdByType pId NodeCorpus
  let cId = maybe (panic "[G.A.N.D.Export] Node has no parent") identity mcId
  docs <- runViewDocuments cId False Nothing Nothing Nothing Nothing Nothing
  pure $ addHeader (T.concat [ "attachment; filename=GarganText_DocsList-"
                             , T.pack $ show pId
                             , ".json"])
    DocumentExport { _de_documents = mapFacetDoc uId <$> docs
                        , _de_garg_version = T.pack $ showVersion PG.version }
  where
    mapFacetDoc uId (FacetDoc { .. }) =
      Document { _d_document =
                 Node { _node_id = facetDoc_id
                      , _node_hash_id = Nothing
                      , _node_typename = toDBid NodeDocument
                      , _node_user_id = uId
                      , _node_parent_id = Nothing
                      , _node_name = facetDoc_title
                      , _node_date = facetDoc_created
                      , _node_hyperdata = facetDoc_hyperdata }
               , _d_ngrams = Ngrams { _ng_ngrams = []
                                    , _ng_hash = "" }
               , _d_hash = "" }
    _mapDoc d = Document { _d_document = d
                         , _d_ngrams   = Ngrams { _ng_ngrams = []
                                                , _ng_hash = "" }
                         , _d_hash     = ""}

getDocumentsCSV :: NodeId
                -- ^ The Node ID of the target user
                -> DocId
                -> GargNoServer (Headers '[Header "Content-Disposition" T.Text] T.Text) -- [Document]
getDocumentsCSV userNodeId pId = do
  dJSON <- getDocumentsJSON userNodeId pId
  let DocumentExport { _de_documents } = getResponse dJSON
  let ret = TE.decodeUtf8 $ BSC.toStrict $ encodeDefaultOrderedByName _de_documents

  pure $ addHeader (T.concat [ "attachment; filename=GarganText_DocsList-"
                             , T.pack $ show pId
                             , ".csv"])
    ret
