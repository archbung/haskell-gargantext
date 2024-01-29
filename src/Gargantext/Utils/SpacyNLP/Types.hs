{-|
Module      : Gargantext.Utils.SpacyNLP.Types
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

module Gargantext.Utils.SpacyNLP.Types where

import Control.Lens
import Data.Aeson.TH (deriveJSON)
import Data.Text hiding (map, group, filter, concat, zip)
import Gargantext.Core.Types (POS(..), NER(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude


data SpacyData = SpacyData { _spacy_data :: ![SpacyText]}
  deriving (Show)

data SpacyText = SpacyText { _spacy_text :: !Text
                           , _spacy_tags :: ![SpacyTags]
                           } deriving (Show)
data SpacyTags =
  SpacyTags { _spacyTags_text :: !Text
            , _spacyTags_text_with_ws :: !Text
            , _spacyTags_whitespace :: !Text
            , _spacyTags_head :: !Text
            , _spacyTags_head_index :: !Int
            , _spacyTags_left_edge :: !Text
            , _spacyTags_right_edge :: !Text
            , _spacyTags_index :: Int
            , _spacyTags_ent_type :: !NER
            , _spacyTags_ent_iob :: !Text
            , _spacyTags_lemma :: !Text
            , _spacyTags_normalized :: !Text
            , _spacyTags_shape :: !Text
            , _spacyTags_prefix :: !Text
            , _spacyTags_suffix :: !Text
            , _spacyTags_is_alpha :: Bool
            , _spacyTags_is_ascii :: Bool
            , _spacyTags_is_digit :: Bool
            , _spacyTags_is_title :: Bool
            , _spacyTags_is_punct :: Bool
            , _spacyTags_is_left_punct :: Bool
            , _spacyTags_is_right_punct :: Bool
            , _spacyTags_is_space :: Bool
            , _spacyTags_is_bracket :: Bool
            , _spacyTags_is_quote :: Bool
            , _spacyTags_is_currency :: Bool
            , _spacyTags_like_url :: Bool
            , _spacyTags_like_num :: Bool
            , _spacyTags_like_email :: Bool
            , _spacyTags_is_oov :: Bool
            , _spacyTags_is_stop :: Bool
            , _spacyTags_pos :: POS
            , _spacyTags_tag :: POS
            , _spacyTags_dep :: !Text
            , _spacyTags_lang :: !Text
            , _spacyTags_prob :: !Int
            , _spacyTags_char_offset :: !Int
            } deriving (Show)


data SpacyRequest = SpacyRequest { _spacyRequest_text :: !Text }
  deriving (Show)

--
-- JSON instances
--

deriveJSON (unPrefix "_spacyTags_")    ''SpacyTags
deriveJSON (unPrefix "_spacy_")        ''SpacyText
deriveJSON (unPrefix "_spacy_")        ''SpacyData
deriveJSON (unPrefix "_spacyRequest_") ''SpacyRequest

--
-- Lenses
--

makeLenses ''SpacyData
makeLenses ''SpacyText
makeLenses ''SpacyTags
makeLenses ''SpacyRequest

