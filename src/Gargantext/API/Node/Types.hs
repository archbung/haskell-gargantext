{-|
Module      : Gargantext.API.Node.Types
Description :
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell    #-}

module Gargantext.API.Node.Types where

import Control.Lens hiding (elements, Empty)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as BSB64
import Data.Swagger
import Data.Text qualified as T
import Gargantext.API.Node.Corpus.New.Types (FileType, FileFormat)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.List.Social (FlowSocialListWith)
import Gargantext.Core.Utils.Prefix (unPrefixSwagger)
import Gargantext.Database.GargDB qualified as GargDB
import Gargantext.Prelude
import Servant.Job.Utils (jsonOptions)
import Web.FormUrlEncoded          (FromForm, ToForm)

-------------------------------------------------------
data NewWithForm = NewWithForm
  { _wf_filetype   :: !FileType
  , _wf_fileformat :: !FileFormat
  , _wf_data       :: !Text    -- NOTE for binary files, this represents base-64 data
  , _wf_lang       :: !(Maybe Lang)
  , _wf_name       :: !Text
  , _wf_selection  :: !FlowSocialListWith
  } deriving (Eq, Show, Generic)

makeLenses ''NewWithForm
instance FromForm NewWithForm
instance ToForm NewWithForm
instance FromJSON NewWithForm where
  parseJSON = genericParseJSON $ jsonOptions "_wf_"
instance ToJSON NewWithForm where
  toJSON = genericToJSON $ jsonOptions "_wf_"
instance ToSchema NewWithForm where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wf_")

-------------------------------------------------------

data NewWithFile = NewWithFile
  { _wfi_b64_data  :: !Text
  , _wfi_lang      :: !(Maybe Lang)
  , _wfi_name      :: !Text
  } deriving (Eq, Show, Generic)

makeLenses ''NewWithFile
instance FromForm NewWithFile
instance ToForm NewWithFile
instance FromJSON NewWithFile where
  parseJSON = genericParseJSON $ jsonOptions "_wfi_"
instance ToJSON NewWithFile where
  toJSON = genericToJSON $ jsonOptions "_wfi_"


instance ToSchema NewWithFile where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wfi_")

instance GargDB.SaveFile NewWithFile where
  saveFile' fp (NewWithFile b64d _ _) = do
    let eDecoded = BSB64.decode $ encodeUtf8 b64d
    case eDecoded of
      Left err -> panicTrace $ T.pack $ "Error decoding: " <> err
      Right decoded -> BS.writeFile fp decoded
    -- BS.writeFile fp $ BSB64.decodeLenient $ TE.encodeUtf8 b64d

--instance GargDB.ReadFile NewWithFile where
--  readFile' = TIO.readFile
