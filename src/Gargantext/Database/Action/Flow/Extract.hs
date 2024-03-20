{-|
Module      : Gargantext.Database.Flow.Extract
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE InstanceSigs            #-}


module Gargantext.Database.Action.Flow.Extract
    where

import Control.Lens ((^.), _Just, view)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as DM
import Gargantext.Core (Lang, NLPServerConfig, PosTagAlgo(CoreNLP))
import Gargantext.Core.Text (HasText(..))
import Gargantext.Core.Text.Corpus.Parsers (splitOn)
import Gargantext.Core.Text.Terms (ExtractNgramsT, ExtractedNgrams(..), TermType, cleanExtractedNgrams, enrichedTerms, extractNgramsT, extractTerms, tt_lang)
import Gargantext.Core.Types (POS(NP), TermsCount)
import Gargantext.Database.Admin.Types.Hyperdata.Contact ( HyperdataContact, cw_lastName, hc_who )
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument, hd_authors, hd_bdd, hd_institutes, hd_source )
import Gargantext.Database.Admin.Types.Node ( Node )
import Gargantext.Database.Prelude (DBCmd)
import Gargantext.Database.Schema.Ngrams ( NgramsType(..), text2ngrams )
import Gargantext.Database.Schema.Node (NodePoly(..))
import Gargantext.Prelude



------------------------------------------------------------------------
instance ExtractNgramsT HyperdataContact
  where
    extractNgramsT _ncs l hc = HashMap.mapKeys (cleanExtractedNgrams 255) <$> extract l hc
      where
        extract :: TermType Lang -> HyperdataContact
                -> DBCmd err (HashMap.HashMap ExtractedNgrams (Map NgramsType Int, TermsCount))
        extract _l hc' = do
          let authors = map text2ngrams
                      $ maybe ["Nothing"] (\a -> [a])
                      $ view (hc_who . _Just . cw_lastName) hc'

          pure $ HashMap.fromList $ [(SimpleNgrams a', (DM.singleton Authors 1, 1)) | a' <- authors ]


-- | Main ngrams extraction functionality.
--   For NgramsTerms, this calls NLP server under the hood.
--   For Sources, Institutes, Authors, this uses simple split on " ".
instance ExtractNgramsT HyperdataDocument
  where
    extractNgramsT :: NLPServerConfig
                   -> TermType Lang
                   -> HyperdataDocument
                   -> DBCmd err (HashMap.HashMap ExtractedNgrams (Map NgramsType Int, TermsCount))
    extractNgramsT ncs lang hd = HashMap.mapKeys (cleanExtractedNgrams 255) <$> extractNgramsT' hd
      where
        extractNgramsT' :: HyperdataDocument
                        -> DBCmd err (HashMap.HashMap ExtractedNgrams (Map NgramsType Int, TermsCount))
        extractNgramsT' doc = do
          let source    = text2ngrams
                        $ maybe "Nothing" identity
                        $ doc ^. hd_source

              institutes = map text2ngrams
                         $ maybe ["Nothing"] (splitOn Institutes (doc^. hd_bdd))
                         $ doc ^. hd_institutes

              authors    = map text2ngrams
                         $ maybe ["Nothing"] (splitOn Authors (doc^. hd_bdd))
                         $ doc ^. hd_authors

          termsWithCounts' <- map (first (enrichedTerms (lang ^. tt_lang) CoreNLP NP)) . concat <$>
                                  liftBase (extractTerms ncs lang $ hasText doc)

          pure $ HashMap.fromList
               $  [(SimpleNgrams source, (DM.singleton Sources     1, 1))                    ]
               <> [(SimpleNgrams     i', (DM.singleton Institutes  1, 1)) | i' <- institutes ]
               <> [(SimpleNgrams     a', (DM.singleton Authors     1, 1)) | a' <- authors    ]
               <> [(EnrichedNgrams   t', (DM.singleton NgramsTerms 1, cnt')) | (t', cnt') <- termsWithCounts'     ]

instance (ExtractNgramsT a, HasText a) => ExtractNgramsT (Node a)
  where
    extractNgramsT ncs l (Node { _node_hyperdata = h }) = extractNgramsT ncs l h


instance HasText a => HasText (Node a)
  where
    hasText (Node { _node_hyperdata = h }) = hasText h


-- Apparently unused functions

-- extractInsert :: ( HasNodeStory env err m
--                  , HasNLPServer env )
--               => [Node HyperdataDocument] -> m ()
-- extractInsert docs = do
--   let documentsWithId = map (\doc -> Indexed (doc ^. node_id) doc) docs
--   let lang = EN
--   ncs <- view $ nlpServerGet lang
--   mapNgramsDocs' <- mapNodeIdNgrams
--                 <$> documentIdWithNgrams
--                     (extractNgramsT ncs $ withLang (Multi lang) documentsWithId)
--                     documentsWithId
--   _ <- insertExtractedNgrams $ HashMap.keys mapNgramsDocs'
--   pure ()


