{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Gargantext.API.GraphQL.NLP
  ( Lang(..)
  , LanguagesArgs(..)
  , LanguagesMap
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

data NLPServer = NLPServer
  {
    server :: !PosTagAlgo
  , url    :: !Text
  }
  deriving (Show, Eq, Generic, GQLType)

resolveLanguages
  :: HasNLPServer env => LanguagesArgs -> GqlM e env LanguagesMap
resolveLanguages ( LanguagesArgs () ) = do
  -- pure $ allLangs
  lift $ do
    ns <- view nlpServer
    printDebug "[resolveLanguages] nlpServer" ns
    pure $ Map.map (\(NLPServerConfig { .. }) -> NLPServer { server
                                                           , url = Protolude.show url }) ns
