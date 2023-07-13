{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Corpus
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Gargantext.Database.Admin.Types.Hyperdata.Corpus
  where

import Gargantext.Prelude
import Gargantext.Core (Lang, defaultLanguage)
import Gargantext.Database.Admin.Types.Hyperdata.CorpusField
import Gargantext.Database.Admin.Types.Hyperdata.Prelude
import PUBMED.Types (APIKey)

------------------------------------------------------------------------
data HyperdataCorpus =
  HyperdataCorpus { _hc_fields         :: [HyperdataField CorpusField]
                  , _hc_pubmed_api_key :: Maybe APIKey
                  -- | The language for the corpus. It defaults to
                  -- 'defaultLanguage' if we don't know which language it is.
                  , _hc_lang           :: !Lang
                  }
    deriving (Generic)

defaultHyperdataCorpus :: HyperdataCorpus
defaultHyperdataCorpus =
  HyperdataCorpus
    { _hc_fields = [ HyperdataField Markdown
                     "Corpus analysis"
                     (MarkdownField "# title\n## subtitle")

                   , HyperdataField JSON
                     "Metadata (Experts only)"
                     (JsonField "Title" "Descr" "Bool query" "Authors")
                   ]
    , _hc_pubmed_api_key = Nothing
    , _hc_lang = defaultLanguage
    }

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Annuaire and Corpus should be the same
data HyperdataAnnuaire = HyperdataAnnuaire { _ha_title        :: !(Maybe Text)
                                           , _ha_desc         :: !(Maybe Text)
                                           } deriving (Show, Generic)

defaultHyperdataAnnuaire :: HyperdataAnnuaire
defaultHyperdataAnnuaire = HyperdataAnnuaire (Just "Title") (Just "Description")

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
instance Hyperdata HyperdataCorpus
instance Hyperdata HyperdataAnnuaire

$(makeLenses ''HyperdataCorpus)
$(makeLenses ''HyperdataAnnuaire)

$(deriveJSON (unPrefix "_hc_") ''HyperdataCorpus)
$(deriveJSON (unPrefix "_ha_") ''HyperdataAnnuaire)

------------------------------------------------------------------------
instance ToSchema HyperdataCorpus where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hc_") proxy
    & mapped.schema.description ?~ "Corpus Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataCorpus

instance ToSchema HyperdataAnnuaire where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_ha_") proxy
    & mapped.schema.description ?~ "Annuaire Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataAnnuaire
------------------------------------------------------------------------
instance Arbitrary HyperdataCorpus where
    arbitrary = pure defaultHyperdataCorpus

instance Arbitrary HyperdataAnnuaire where
  arbitrary = pure defaultHyperdataAnnuaire
------------------------------------------------------------------------
instance FromField HyperdataCorpus
  where
    fromField = fromField'

instance FromField HyperdataAnnuaire
  where
    fromField = fromField'
------------------------------------------------------------------------
instance DefaultFromField SqlJsonb HyperdataCorpus
  where
    defaultFromField = fromPGSFromField

instance DefaultFromField SqlJsonb HyperdataAnnuaire
  where
    defaultFromField = fromPGSFromField
