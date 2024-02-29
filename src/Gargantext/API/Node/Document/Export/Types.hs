{-|
Module      : Gargantext.API.Node.Document.Export.Types
Description : Types for Gargantext.API.Node.Document.Export
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Gargantext.API.Node.Document.Export.Types where

import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON)
import Data.Csv (DefaultOrdered(..), ToNamedRecord(..), (.=), header, namedRecord)
import Data.Swagger ( genericDeclareNamedSchema, ToParamSchema(..), ToSchema(..) )
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Gargantext.Core.Types ( Node, TODO )
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument(..) )
import Gargantext.Database.Admin.Types.Node (DocId)
import Gargantext.Database.Schema.Node (NodePoly(..))
import Gargantext.Utils.Servant (ZIP)
import Gargantext.Utils.Zip (zipContentsPure)
import Protolude
import Servant ((:>), (:<|>), Get, Header, Headers(..), JSON, MimeRender(..), PlainText, Summary)


-- | Document Export
data DocumentExport =
  DocumentExport { _de_documents    :: [Document]
                 , _de_garg_version :: Text
                 } deriving (Generic)

-- | This is to represent a zipped document export. We want to have doc_id in zipped file name.
data DocumentExportZIP =
  DocumentExportZIP { _dez_dexp   :: DocumentExport
                    , _dez_doc_id :: DocId } deriving (Generic)
    
    
data Document =
  Document { _d_document :: Node HyperdataDocument
           , _d_ngrams   :: Ngrams
           , _d_hash     :: Hash
           } deriving (Generic)

--instance Read Document where
--  read "" = panic "not implemented"
instance DefaultOrdered Document where
  headerOrder _ = header ["Publication Day"
                         , "Publication Month"
                         , "Publication Year"
                         , "Authors"
                         , "Title"
                         , "Source"
                         , "Abstract"]
instance ToNamedRecord Document where
  toNamedRecord (Document { _d_document = Node { .. }}) =
    namedRecord
    [ "Publication Day" .= _hd_publication_day _node_hyperdata
    , "Publication Month" .= _hd_publication_month _node_hyperdata
    , "Publication Year" .= _hd_publication_year _node_hyperdata
    , "Authors" .= _hd_authors _node_hyperdata
    , "Title" .= _hd_title _node_hyperdata
    , "Source" .= (TE.encodeUtf8 <$> _hd_source _node_hyperdata)
    , "Abstract" .= (TE.encodeUtf8 <$> _hd_abstract _node_hyperdata) ]

data Ngrams =
  Ngrams { _ng_ngrams :: [Text]
         , _ng_hash   :: Hash
         } deriving (Generic)

type Hash = Text
-------
instance ToSchema DocumentExport where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_de_")

instance ToSchema DocumentExportZIP where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_dez_")
  
instance ToSchema Document where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_d_")

instance ToSchema Ngrams where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_ng_")

-------
instance ToParamSchema DocumentExport where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

instance ToParamSchema DocumentExportZIP where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

instance ToParamSchema Document where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

instance ToParamSchema Ngrams where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)
--------------------------------------------------
type API = Summary "Document Export"
            :> "export"
            :> ( "json"
                 :> Get '[JSON] (Headers '[Servant.Header "Content-Disposition" Text] DocumentExport)
               :<|> "json.zip"
                 :> Get '[ZIP] (Headers '[Servant.Header "Content-Disposition" Text] DocumentExportZIP)
               :<|> "csv"
                 :> Get '[PlainText] (Headers '[Servant.Header "Content-Disposition" Text] Text) )

$(deriveJSON (unPrefix "_ng_") ''Ngrams)
$(deriveJSON (unPrefix "_d_") ''Document)
$(deriveJSON (unPrefix "_de_") ''DocumentExport)



------

-- Needs to be here because of deriveJSON TH above

dezFileName :: DocumentExportZIP -> Text
dezFileName (DocumentExportZIP { .. }) = "GarganText_DocsList-" <> show _dez_doc_id <> ".json"
    
instance MimeRender ZIP DocumentExportZIP where
  mimeRender _ dexpz@(DocumentExportZIP { .. }) =
    zipContentsPure (T.unpack $ dezFileName dexpz) (encode _dez_dexp)
