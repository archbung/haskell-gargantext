{-|
Module      : Gargantext.Core.Viz.Chart
Description : Graph utils
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Core.Viz.Chart
  where

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Map.Strict (toList)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Gargantext.API.Ngrams.NgramsTree ( toTree, NgramsTree )
import Gargantext.API.Ngrams.Tools ( filterListWithRoot, getListNgrams, getRepo, mapTermListRoot )
import Gargantext.API.Ngrams.Types ( NgramsTerm(NgramsTerm) )
import Gargantext.Core.NodeStory.Types ( HasNodeStory )
import Gargantext.Core.Text.Metrics.Count (occurrencesWith)
import Gargantext.Core.Text.Ngrams (NgramsType)
import Gargantext.Core.Types.Main ( ListType )
import Gargantext.Database.Admin.Types.Node ( NodeType(NodeList), CorpusId, contextId2NodeId )
import Gargantext.Core.Viz.Types ( Histo(Histo) )
import Gargantext.Database.Action.Metrics.NgramsByContext ( countContextsByNgramsWith, getContextsByNgramsOnlyUser )
import Gargantext.Database.Admin.Config ( userMaster )
import Gargantext.Database.Prelude (DBCmd)
import Gargantext.Database.Query.Table.Node ( getListsWithParentId )
import Gargantext.Database.Query.Table.Node.Select ( selectNodesWithUsername )
import Gargantext.Database.Query.Table.NodeContext (selectDocsDates)
import Gargantext.Database.Schema.Node ( NodePoly(_node_id) )
import Gargantext.Prelude hiding (toList)


histoData :: CorpusId -> DBCmd err Histo
histoData cId = do
  dates <- selectDocsDates cId
  let (ls, css) = V.unzip
                $ V.fromList
                $ sortOn fst -- TODO Vector.sortOn
                $ toList
                $ occurrencesWith identity dates
  pure (Histo ls css)


chartData :: HasNodeStory env err m
          => CorpusId -> NgramsType -> ListType
          -> m Histo
chartData cId nt lt = do
  ls' <- selectNodesWithUsername NodeList userMaster
  ls <- map (_node_id) <$> getListsWithParentId cId
  ts <- mapTermListRoot ls nt <$> getRepo ls
  let
    dico = filterListWithRoot [lt] ts
    terms = catMaybes $ List.concat $ map (\(a,b) -> [Just a, b]) $ HashMap.toList dico
    group' dico' x = case HashMap.lookup x dico' of
        Nothing -> x
        Just x' -> maybe x identity x'

  (_total,mapTerms) <- countContextsByNgramsWith (group' dico)
                    <$> getContextsByNgramsOnlyUser cId (ls' <> ls) nt terms
  let (dates, count) = V.unzip $
                       V.fromList $
                       List.sortOn snd $
                       (\(NgramsTerm t,(d,_)) -> (t, d)) <$>
                       HashMap.toList mapTerms
  pure (Histo dates (round <$> count))


treeData :: HasNodeStory env err m
        => CorpusId -> NgramsType -> ListType
        -> m (V.Vector NgramsTree)
treeData cId nt lt = do
  ls' <- selectNodesWithUsername NodeList userMaster
  ls <- map (_node_id) <$> getListsWithParentId cId
  ts <- mapTermListRoot ls nt <$> getRepo ls

  let
    dico = filterListWithRoot [lt] ts
    terms = catMaybes $ List.concat $ map (\(a,b) -> [Just a, b]) $ HashMap.toList dico

  -- FIXME(adn) Audit the usage, as we are converting between a context id to a node id.
  cs' <- HashMap.map (Set.map contextId2NodeId) <$> getContextsByNgramsOnlyUser cId (ls' <> ls) nt terms

  m  <- getListNgrams ls nt
  pure $ V.fromList $ toTree lt cs' m
