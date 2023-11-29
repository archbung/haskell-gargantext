{-|
Module      : Gargantext.Core.Viz.Graph
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

See
https://martin.hoppenheit.info/blog/2023/xml-stream-processing-with-haskell/
for a tutorial of xml-conduit rendering.

-}


{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedLists   #-}   -- allows to write Map and HashMap as lists
{-# LANGUAGE TypeOperators     #-}

module Gargantext.Core.Viz.Graph.GEXF
  where

import Conduit
import Data.Conduit.Combinators qualified as CC
import Data.XML.Types qualified as XML
import Gargantext.Core.Viz.Graph.Types qualified as G
import Gargantext.Prelude
import Prelude qualified
import Servant (MimeRender(..), MimeUnrender(..))
import Servant.XML.Conduit (XML)
import Text.XML.Stream.Render qualified as XML


-- Converts to GEXF format
-- See https://gephi.org/gexf/format/
graphToXML :: Monad m => G.Graph -> ConduitT i XML.Event m ()
graphToXML (G.Graph { .. }) = root _graph_nodes _graph_edges
  where
    -- root :: [G.Node] -> [G.Edge] -> ConduitT i XML.Event m ()
    root gn ge =
      XML.tag "gexf" params $ meta <> (graph gn ge)
        where
          params =    XML.attr "xmlns" "http://www.gexf.net/1.3"
                   <> XML.attr "version" "1.3"

    meta = XML.tag "meta" params $ creator <> desc
      where
        params = XML.attr "lastmodifieddate" "2020-03-13"
        creator = XML.tag "creator" mempty $ XML.content "Gargantext.org"
        desc = XML.tag "description" mempty $ XML.content "Gargantext gexf file"

    graph :: (Monad m) => [G.Node] -> [G.Edge] -> ConduitT i XML.Event m ()
    graph gn ge = XML.tag "graph" params $ (nodes gn) <> (edges ge)
      where
        params = XML.attr "mode" "static"
              <> XML.attr "defaultedgetype" "directed"

    nodes :: (Monad m) => [G.Node] -> ConduitT i XML.Event m ()
    nodes gn = XML.tag "nodes" mempty (yieldMany gn .| awaitForever node')
    node' :: (Monad m) => G.Node -> ConduitT i XML.Event m ()
    node' (G.Node { .. }) = XML.tag "node" params (XML.tag "viz:size" sizeParams $ XML.content "")
      where
        params = XML.attr "id" node_id
              <> XML.attr "label" node_label
        sizeParams = XML.attr "value" (show node_size)

    edges :: (Monad m) => [G.Edge] -> ConduitT i XML.Event m ()
    edges ge = XML.tag "edges" mempty (yieldMany ge .| awaitForever edge')
    edge' :: (Monad m) => G.Edge -> ConduitT i XML.Event m ()
    edge' (G.Edge { .. }) = XML.tag "edge" params $ XML.content ""
      where
        params = XML.attr "id" edge_id
              <> XML.attr "source" edge_source
              <> XML.attr "target" edge_target
              <> XML.attr "weight" (show edge_weight)


instance MimeRender XML G.Graph where
  mimeRender _ g = runConduitPure (source .| CC.sinkLazyBuilder)
    where
      namespaces = [ ("viz", "http://gexf.net/1.3/viz")
                   , ("xsi", "http://www.w3.org/2001/XMLSchema-instance")
                   , ("schemaLocation", "http://gexf.net/1.3") ]
      source = graphToXML g .| XML.renderBuilder (XML.def { XML.rsNamespaces = namespaces })
      --encoded = source .| mapC TE.encodeUtf8


-- just to be able to derive a client for the entire gargantext API,
-- we however want to avoid sollicitating this instance
instance MimeUnrender XML G.Graph where
  mimeUnrender _ _ = Prelude.error "MimeUnrender Graph: not defined, just a placeholder"
