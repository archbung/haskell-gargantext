{-|
Module      : Gargantext.Database.Flow.Types
Description : Types for Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE TemplateHaskell         #-}

module Gargantext.Database.Action.Flow.Types
    where

import Conduit (ConduitT)
import Control.Lens (makeLenses)
import Data.Aeson (ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.HashMap.Strict (HashMap)
import Data.Swagger (ToSchema(..), genericDeclareNamedSchema)
import Gargantext.Core.Flow.Types ( UniqId )
import Gargantext.Core.NodeStory.Types ( HasNodeStory )
import Gargantext.Core.Text ( HasText )
import Gargantext.API.Admin.Orchestrator.Types qualified as API
import Gargantext.Core.Text.Ngrams (NgramsType(..))
import Gargantext.Core.Text.Terms ( ExtractNgramsT )
import Gargantext.Core.Types (HasValidationError, TermsCount)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument )
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Database.Prelude (CmdM)
import Gargantext.Database.Query.Table.Node.Document.Insert ( UniqParameters, InsertDb, ToNode, AddUniqId )
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Tree.Error (HasTreeError)
import Gargantext.Database.Types (Indexed)
import Gargantext.Prelude
import Gargantext.System.Logging ( MonadLogger )


type FlowCmdM env err m =
  ( CmdM     env err m
  , HasNodeStory env err m
  , HasNodeError err
  , HasValidationError err
  , HasTreeError err
  , MonadLogger m
  )

type FlowCorpus a = ( UniqParameters a
                    , InsertDb       a
                    , ExtractNgramsT a
                    , HasText        a
                    , ToNode         a
                    , ToJSON         a
                    )

type FlowInsertDB a = ( AddUniqId a
                      , UniqId    a
                      , UniqParameters a
                      , InsertDb  a
                      )



data DocumentIdWithNgrams a b =
     DocumentIdWithNgrams
     { documentWithId :: Indexed NodeId a
     , documentNgrams :: HashMap b (Map NgramsType Int, TermsCount)
     } deriving (Show)


-- TODO use internal with API name (could be old data)
data DataOrigin = InternalOrigin { _do_api :: API.ExternalAPIs }
                | ExternalOrigin { _do_api :: API.ExternalAPIs }
               -- TODO Web
  deriving (Generic, Eq)

makeLenses ''DataOrigin
deriveJSON (unPrefix "_do_") ''DataOrigin
instance ToSchema DataOrigin where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_do_")

data DataText = DataOld ![NodeId]
              | DataNew !(Maybe Integer, ConduitT () HyperdataDocument IO ())
              --- | DataNew ![[HyperdataDocument]]
