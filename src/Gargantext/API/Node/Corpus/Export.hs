{-|
Module      : Gargantext.API.Node.Corpus.Export
Description : Corpus export
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Main exports of Gargantext:
- corpus
- document and ngrams
- lists
-}

module Gargantext.API.Node.Corpus.Export
  where

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (pack)
import Gargantext.API.Ngrams.Tools (filterListWithRoot, mapTermListRoot, getRepo)
import Gargantext.API.Ngrams.Types ( NgramsTerm(unNgramsTerm) )
import Gargantext.API.Node.Corpus.Export.Types ( Corpus(..) )
import Gargantext.API.Node.Document.Export.Types qualified as DocumentExport
import Gargantext.API.Prelude (GargNoServer)
import Gargantext.Core.NodeStory.Types ( NodeListStory )
import Gargantext.Core.Types
import Gargantext.Database.Action.Metrics.NgramsByContext (getNgramsByContextOnlyUser)
import Gargantext.Database.Admin.Config (userMaster)
import Gargantext.Database.Admin.Types.Hyperdata.Document ( HyperdataDocument(..) )
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.Node ( defaultList )
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.Node.Select (selectNodesWithUsername)
import Gargantext.Database.Query.Table.NodeContext (selectDocNodes)
import Gargantext.Database.Schema.Context (_context_id)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude hiding (hash)
import Gargantext.Prelude.Crypto.Hash (hash)
import Servant (Headers, Header, addHeader)

--------------------------------------------------
-- | Hashes are ordered by Set
getCorpus :: CorpusId
          -> Maybe ListId
          -> Maybe NgramsType
          -> GargNoServer (Headers '[Header "Content-Disposition" Text] Corpus)
getCorpus cId lId nt' = do

  let
    nt = fromMaybe NgramsTerms nt'

  listId <- case lId of
    Nothing -> defaultList cId
    Just l  -> pure l

  -- FIXME(adn) Audit the usage of this, we are converting from a node
  -- to a context id.
  ns   <- Map.fromList
       <$> map (\n -> (nodeId2ContextId $ _context_id n, n))
       <$> selectDocNodes cId

  repo <- getRepo [listId]
  ngs  <- getContextNgrams cId listId MapTerm nt repo
  let  -- uniqId is hash computed already for each document imported in database
    r = Map.intersectionWith
        (\a b -> DocumentExport.Document { _d_document = context2node a
                                         , _d_ngrams = DocumentExport.Ngrams (Set.toList b) (hash b)
                                         , _d_hash = d_hash a b }
        ) ns (Map.map (Set.map unNgramsTerm) ngs)
          where
            d_hash :: Context HyperdataDocument -> Set Text -> Text
            d_hash  _a b = hash [ -- fromMaybe "" (_hd_uniqId $ _context_hyperdata a),
                                hash b
                               ]
  pure $ addHeader ("attachment; filename=GarganText_corpus-" <> pack (show cId) <> ".json")
    $ Corpus { _c_corpus = Map.elems r
             , _c_hash = hash $ List.map DocumentExport._d_hash $ Map.elems r }

getContextNgrams :: HasNodeError err
        => CorpusId
        -> ListId
        -> ListType
        -> NgramsType
        -> NodeListStory
        -> Cmd err (Map ContextId (Set NgramsTerm))
getContextNgrams cId lId listType nt repo = do
--  lId <- case lId' of
--    Nothing -> defaultList cId
--    Just  l -> pure l

  lIds <- selectNodesWithUsername NodeList userMaster
  let ngs = filterListWithRoot [listType] $ mapTermListRoot [lId] nt repo
  -- TODO HashMap
  r <- getNgramsByContextOnlyUser cId (lIds <> [lId]) nt (HashMap.keys ngs)
  pure r

-- TODO
-- Exports List
-- Version number of the list
