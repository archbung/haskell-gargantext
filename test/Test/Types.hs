module Test.Types where

import Data.Aeson ((.:), (.:?), (.=), FromJSON(..), ToJSON(..), object, withObject)
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..))
import Gargantext.Prelude


data JobPollHandle = JobPollHandle {
    _jph_id     :: !Text
  , _jph_log    :: [JobLog]
  , _jph_status :: !Text
  , _jph_error  :: !(Maybe Text)
    } deriving Show

instance FromJSON JobPollHandle where
  parseJSON = withObject "JobPollHandle" $ \o -> do
    _jph_id     <- o .: "id"
    _jph_log    <- o .: "log"
    _jph_status <- o .: "status"
    _jph_error  <- o .:? "error"
    pure JobPollHandle{..}

instance ToJSON JobPollHandle where
  toJSON JobPollHandle{..} = object [
      "id"      .= toJSON _jph_id
    , "log"     .= toJSON _jph_log
    , "status"  .= toJSON _jph_status
    , "error"   .= toJSON _jph_error
    ]
