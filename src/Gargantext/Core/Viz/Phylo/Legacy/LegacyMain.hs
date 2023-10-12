{-|
Module      : Gargantext.Core.Viz.Phylo.Main
Description : Phylomemy Main
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Gargantext.Core.Viz.Phylo.Legacy.LegacyMain
  where

import Control.Lens hiding (Level)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Proxy
import Data.Set qualified as Set
import Data.Text qualified as Text
import Gargantext.API.Ngrams.Tools (getTermsWith)
import Gargantext.API.Ngrams.Types
import Gargantext.Core (HasDBid, withDefaultLanguage)
import Gargantext.Core.NodeStory (HasNodeStory)
import Gargantext.Core.Text.Context (TermList)
import Gargantext.Core.Text.Terms.WithList
import Gargantext.Core.Types
import Gargantext.Core.Viz.LegacyPhylo hiding (Svg, Dot)
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Query.Table.Node(defaultList, getNodeWith)
import Gargantext.Database.Query.Table.NodeContext (selectDocs)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Database.Schema.Node
import Gargantext.Prelude hiding (to)

type MinSizeBranch = Int

flowPhylo :: (HasNodeStory env err m, HasDBid NodeType)
          => CorpusId
          -> m Phylo
flowPhylo cId = do

  corpus_node <- getNodeWith cId (Proxy @ HyperdataCorpus)
  let lang = withDefaultLanguage $ view (node_hyperdata . to _hc_lang) corpus_node
  list'    <- defaultList cId
  termList <- HashMap.toList <$> getTermsWith (Text.words . unNgramsTerm) [list'] NgramsTerms (Set.singleton MapTerm)

  docs' <- catMaybes
        <$> map (\h -> (,) <$> _hd_publication_year h
                           <*> _hd_abstract h
                )
        <$> selectDocs cId

  let
    patterns = buildPatterns termList
    -- | To filter the Ngrams of a document based on the termList
    filterTerms :: Patterns -> (Date, Text) -> (Date, [Text])
    filterTerms patterns' (y,d) = (y, fst <$> termsInText lang patterns' d)

    docs = map ((\(y,t) -> Document y t) . filterTerms patterns) docs'

  --liftBase $ flowPhylo' (List.sortOn date docs) termList l m fp
  pure $ buildPhylo (List.sortOn date docs) termList


-- TODO SortedList Document
flowPhylo' :: [Document] -> TermList      -- ^Build
           -> Level       -> MinSizeBranch -- ^View
           -> FilePath
           -> IO FilePath
flowPhylo' corpus terms l m fp = do
  let
    phylo = buildPhylo corpus terms
    phVie = viewPhylo  l m phylo

  writePhylo fp phVie


defaultQuery :: PhyloQueryBuild
defaultQuery = undefined
-- defaultQuery = defaultQueryBuild'
--   "Default Title"
--   "Default Description"

buildPhylo :: [Document] -> TermList -> Phylo
buildPhylo = trace (show defaultQuery :: Text) $ buildPhylo' defaultQuery

buildPhylo' :: PhyloQueryBuild -> [Document] -> TermList -> Phylo
buildPhylo' _ _ _ = undefined
-- buildPhylo' q corpus termList = toPhylo q corpus termList Map.empty

-- refactor 2021
-- queryView :: Level -> MinSizeBranch -> PhyloQueryView
-- queryView level _minSizeBranch = PhyloQueryView level Merge False 2
--            [BranchAge]
--            []
--            -- [SizeBranch $ SBParams minSizeBranch]
--            [BranchPeakFreq,GroupLabelCooc]
--            (Just (ByBranchAge,Asc))
--            Json Flat True

queryView :: Level -> MinSizeBranch -> PhyloQueryView
queryView _level _minSizeBranch = undefined

viewPhylo :: Level -> MinSizeBranch -> Phylo -> PhyloView
viewPhylo _l _b _phylo = undefined
-- viewPhylo l b phylo = toPhyloView (queryView l b) phylo

writePhylo :: FilePath -> PhyloView -> IO FilePath
writePhylo _fp _phview = undefined
-- writePhylo fp phview = runGraphviz (viewToDot phview) Svg fp

-- refactor 2021
-- viewPhylo2Svg :: PhyloView -> IO DB.ByteString
-- viewPhylo2Svg p = graphvizWithHandle Dot (viewToDot p) Svg DB.hGetContents
