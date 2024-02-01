{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Gargantext.API.GraphQL.NLP
  ( Lang(..)
  , LanguagesArgs(..)
  , LanguagesMap
  , LanguageTuple
  , resolveLanguages
  )
  where

import Control.Lens (view)
import Data.Morpheus.Types (GQLType)
import Gargantext.API.GraphQL.Types
import Gargantext.Core (Lang(..), NLPServerConfig(..), PosTagAlgo)  -- , allLangs)
import Gargantext.Core.NLP (HasNLPServer(..))
import Gargantext.Prelude
import Protolude
import qualified Data.Map.Strict as Map

newtype LanguagesArgs
  = LanguagesArgs ()
    deriving stock (Generic)
    deriving anyclass (GQLType)

type LanguagesMap = Map.Map Lang NLPServer
data LanguageTuple =
  LanguageTuple { lt_lang   :: Lang
                , lt_server :: NLPServer }
    deriving stock (Generic)
    deriving anyclass (GQLType)

data NLPServer = NLPServer
  {
    server :: !PosTagAlgo
  , url    :: !Text
  }
  deriving (Show, Eq, Generic, GQLType)

resolveLanguages
  :: HasNLPServer env => GqlM e env [LanguageTuple]
resolveLanguages = do
  lift $ do
    ns <- view nlpServer
    printDebug "[resolveLanguages] nlpServer" ns
    pure $ [LanguageTuple { lt_lang = lang
                          , lt_server = NLPServer { server, url = Protolude.show url } }
           | (lang, NLPServerConfig { .. }) <- Map.toList ns]
