{-|
Module      : Gargantext.Database.Schema.NgramsPostag
Description : Ngrams connection to the Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Each Ngrams has a pos-tagging version to ease the default groups of
ngrams in NgramsTerm Lists.

-}

{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.NgramsPostag
  where

import Control.Lens ( makeLenses )
import Database.PostgreSQL.Simple qualified as PGS
import Gargantext.Database.Schema.Prelude ( Column, SqlInt4, SqlText, ToField(toField), toRow )
import Gargantext.Prelude


data NgramsPostagPoly id
                      lang_id
                      algo_id
                      postag
                      ngrams_id
                      lemm_id
                      score
  = NgramsPostagPoly { _ngramsPostag_id        :: !id
                     , _ngramsPostag_lang_id   :: !lang_id
                     , _ngramsPostag_algo_id   :: !algo_id
                     , _ngramsPostag_postag    :: !postag
                     , _ngramsPostag_ngrams_id :: !ngrams_id
                     , _ngramsPostag_lemm_id   :: !lemm_id
                     , _ngramsPostag_score     :: !score
                     } deriving (Show)

------------------------------------------------------------------------
data PosTag = PosTag { unPosTag :: !Text }
            | NER    { unNER    :: !Text } -- TODO

------------------------------------------------------------------------
-- type NgramsPostag   = NgramsPostagPoly (Maybe Int) Lang PostTagAlgo (Maybe PosTag) NgramsTerm NgramsTerm (Maybe Int)
type NgramsPostagDB = NgramsPostagPoly (Maybe Int) Int Int (Maybe Text) Int Int Int

------------------------------------------------------------------------
type NgramsPosTagWrite = NgramsPostagPoly (Maybe (Column SqlInt4))
                                          (Column SqlInt4)
                                          (Column SqlInt4)
                                          (Maybe (Column SqlText))
                                          (Column SqlInt4)
                                          (Column SqlInt4)
                                          (Maybe (Column SqlInt4))

type NgramsPosTagRead  = NgramsPostagPoly (Column SqlInt4)
                                          (Column SqlInt4)
                                          (Column SqlInt4)
                                          (Column SqlText)
                                          (Column SqlInt4)
                                          (Column SqlInt4)
                                          (Column SqlInt4)
makeLenses ''NgramsPostagPoly

instance PGS.ToRow NgramsPostagDB where
  toRow (NgramsPostagPoly f0 f1 f2 f3 f4 f5 f6) = [ toField f0
                                                  , toField f1
                                                  , toField f2
                                                  , toField f3
                                                  , toField f4
                                                  , toField f5
                                                  , toField f6
                                                  ]
