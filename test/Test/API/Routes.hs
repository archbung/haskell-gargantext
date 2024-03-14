{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators #-}


module Test.API.Routes where

import Fmt (Builder, (+|), (|+))
import Gargantext.API.Admin.Auth.Types (AuthRequest, AuthResponse)
import Gargantext.API.Ngrams (TableNgramsApiGet, TableNgramsApiPut)
import Gargantext.API.Ngrams.Types ( NgramsTable, NgramsTablePatch, OrderBy, TabType, Versioned, VersionedWithCount )
import Gargantext.API.Routes (AuthAPI, GargAPIVersion, MkGargAPI)
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Core.Types.Main (ListType)
import Gargantext.Core.Types.Query (Limit, MaxSize, MinSize, Offset)
import Gargantext.Prelude
import Network.Wai.Handler.Warp (Port)
import Servant ((:>), Capture)
import Servant.Client (ClientM, client)


-- This is for requests made by http.client directly to hand-crafted URLs    
curApi :: Builder
curApi = "v1.0"

mkUrl :: Port -> Builder -> ByteString
mkUrl _port urlPiece =
  "/api/" +| curApi |+ urlPiece



-- This is for Servant.Client requests
    
auth_api :: AuthRequest -> ClientM AuthResponse
auth_api = client (Proxy :: Proxy (MkGargAPI (GargAPIVersion AuthAPI)))

-- | Shortcut for TableNgramsApiGet full path
type APITableNgramsGet = MkGargAPI (GargAPIVersion ( "node"
                                                   :> Capture "node_id" NodeId
                                                   :> "ngrams"
                                                   :> TableNgramsApiGet ) )
table_ngrams_get_api :: NodeId
                     -> TabType
                     -> NodeId
                     -> Limit
                     -> Maybe Offset
                     -> Maybe ListType
                     -> Maybe MinSize
                     -> Maybe MaxSize
                     -> Maybe OrderBy
                     -> Maybe Text
                     -> ClientM (VersionedWithCount NgramsTable)
table_ngrams_get_api = client (Proxy :: Proxy APITableNgramsGet)
type APITableNgramsPut = MkGargAPI (GargAPIVersion ( "node"
                                                   :> Capture "node_id" NodeId
                                                   :> "ngrams"
                                                   :> TableNgramsApiPut ) )
table_ngrams_put_api :: NodeId
                     -> TabType
                     -> NodeId
                     -> Versioned NgramsTablePatch
                     -> ClientM (Versioned NgramsTablePatch)
table_ngrams_put_api = client (Proxy :: Proxy APITableNgramsPut)
