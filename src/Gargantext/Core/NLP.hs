{-|
Module      : Gargantext.Core.NLP
Description : GarganText NLP
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Core.NLP where

import Control.Lens (Getter, at, non)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Network.URI (URI(..), parseURI)
import Gargantext.Core (Lang(..), NLPServerConfig(..), PosTagAlgo(..), allLangs)
import Gargantext.Prelude.NLP.Types (NLPConfig(..))
import Gargantext.Utils.Tuple (uncurryMaybeSecond)
import Protolude hiding (All)


type NLPServerMap = Map.Map Lang NLPServerConfig

class HasNLPServer env where
  nlpServer :: Getter env NLPServerMap
  nlpServerGet :: Lang -> Getter env NLPServerConfig
  -- default implementation
  nlpServerGet l = nlpServer . at l . non defaultNLPServer

defaultNLPServer :: NLPServerConfig
defaultNLPServer = NLPServerConfig { server = CoreNLP
                                   , url = fromJust $ parseURI "http://localhost:9000"
                                   }

nlpServerConfigFromURI :: URI -> Maybe NLPServerConfig
nlpServerConfigFromURI uri@(URI { uriScheme = "corenlp:" }) =
  Just $ NLPServerConfig { server = CoreNLP
                         , url = uri { uriScheme = "http:" }
                         }
nlpServerConfigFromURI uri@(URI { uriScheme = "corenlps:" }) =
  Just $ NLPServerConfig { server = CoreNLP
                         , url = uri { uriScheme = "https:" }
                         }
nlpServerConfigFromURI uri@(URI { uriScheme = "johnsnow:" }) =
  Just $ NLPServerConfig { server = JohnSnowServer
                         , url = uri { uriScheme = "http:" }
                         }
nlpServerConfigFromURI uri@(URI { uriScheme = "johnsnows:" }) =
  Just $ NLPServerConfig { server = JohnSnowServer
                         , url = uri { uriScheme = "https:" }
                         }
nlpServerConfigFromURI uri@(URI { uriScheme = "spacy:" }) =
  Just $ NLPServerConfig { server = Spacy
                         , url = uri { uriScheme = "http:" }
                         }
nlpServerConfigFromURI uri@(URI { uriScheme = "spacys:" }) =
  Just $ NLPServerConfig { server = Spacy
                         , url = uri { uriScheme = "https:" }
                         }
nlpServerConfigFromURI _ = Nothing


nlpServerMap :: NLPConfig -> NLPServerMap
nlpServerMap (NLPConfig { .. }) =
  Map.fromList $ catMaybes $
    [ uncurryMaybeSecond (EN, nlpServerConfigFromURI _nlp_default) ] ++
    ((\lang ->
        uncurryMaybeSecond (lang, Map.lookup (show lang) _nlp_languages >>= nlpServerConfigFromURI ))
      <$> allLangs)
