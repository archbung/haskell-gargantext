{-|
Module      : Gargantext.Utils.SpacyNLP
Description : John Snow NLP API connexion
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Spacy ecosystem: https://github.com/explosion/spaCy

Server to be used: https://gitlab.iscpif.fr/gargantext/spacy-server

-}

{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Utils.SpacyNLP (
    module Gargantext.Utils.SpacyNLP.Types
  , spacyRequest
  , spacyTagsToToken
  , spacyDataToPosSentences
  , nlp
  ) where

import Data.Aeson (encode)
import Data.Text hiding (map, group, filter, concat, zip)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Terms.Multi.PosTagging.Types
import Gargantext.Prelude
import Network.HTTP.Simple (parseRequest, httpJSON, setRequestBodyLBS, getResponseBody, Response)
import Network.URI (URI(..))
import Gargantext.Utils.SpacyNLP.Types


spacyRequest :: URI -> Text -> IO SpacyData
spacyRequest uri txt = do
  req <- parseRequest $ "POST " <> show (uri { uriPath = "/pos" })
  let request = setRequestBodyLBS (encode $ SpacyRequest txt) req
  result <- httpJSON request :: IO (Response SpacyData)
  pure $ getResponseBody result

----------------------------------------------------------------
spacyTagsToToken :: SpacyTags -> Token
spacyTagsToToken st = Token (_spacyTags_index st)
                   (_spacyTags_normalized st)
                   (_spacyTags_text st)
                   (_spacyTags_lemma st)
                   (_spacyTags_head_index st)
                   (_spacyTags_char_offset st)
                   (Just $ _spacyTags_pos st)
                   (Just $ _spacyTags_ent_type st)
                   (Just $ _spacyTags_prefix st)
                   (Just $ _spacyTags_suffix st)

spacyDataToPosSentences :: SpacyData -> PosSentences
spacyDataToPosSentences (SpacyData ds) = PosSentences
   $  map (\(i, ts) -> Sentence i ts)
   $ zip [1..]
   $ map (\(SpacyText _ tags)-> map spacyTagsToToken tags) ds

-----------------------------------------------------------------

nlp :: URI -> Lang -> Text -> IO PosSentences
nlp uri _lang txt = spacyDataToPosSentences <$> spacyRequest uri txt
