{-|
Module      : Gargantext.Database.Query.Facet
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
------------------------------------------------------------------------
module Gargantext.Database.Query.Facet
  ( runViewAuthorsDoc
  , runViewDocuments
  , viewDocuments
  , runCountDocuments
  , filterWith

  , Category
  , Score
  , Title

  , Pair(..)
  , Facet(..)
  , FacetDoc
  , FacetDocRead
  , FacetPaired(..)
  , FacetPairedRead
  , OrderBy(..)
  )
  where

import Control.Arrow (returnA)
import Control.Lens ((^.))
import Data.Text qualified as T
import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Core.Types.Query (Limit, Offset, IsTrash)
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Facet.Types
import Gargantext.Database.Query.Filter
import Gargantext.Database.Query.Table.Context
import Gargantext.Database.Query.Table.ContextNodeNgrams
import Gargantext.Database.Query.Table.Ngrams
import Gargantext.Database.Query.Table.Node (defaultList)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.NodeContext (queryNodeContextTable)
import Gargantext.Database.Schema.Context
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.NodeContext
import Opaleye
import Opaleye.Aggregate qualified as OAgg
import Opaleye.Internal.Unpackspec ()
import Protolude hiding (null, map, sum, not)

------------------------------------------------------------------------


-- TODO-SECURITY check
runViewAuthorsDoc :: HasDBid NodeType
                  => ContactId
                  -> IsTrash
                  -> Maybe Offset
                  -> Maybe Limit
                  -> Maybe OrderBy
                  -> Cmd err [FacetDoc]
runViewAuthorsDoc cId t o l order = runOpaQuery $ filterWith o l order $ viewAuthorsDoc cId t ntId
  where
    ntId = NodeDocument

-- TODO add delete ?
viewAuthorsDoc :: HasDBid NodeType
               => ContactId
               -> IsTrash
               -> NodeType
               -> Select FacetDocRead
viewAuthorsDoc cId _ nt = proc () -> do
  --(doc,(_,(_,(_,contact')))) <- queryAuthorsDoc      -< ()
  (doc, _, _, _, contact') <- queryAuthorsDoc -< ()

  restrict -< fromMaybeFields (sqlInt4 $ -1) (_node_id <$> contact') .=== pgNodeId cId
  restrict -< _node_typename doc   .== sqlInt4 (toDBid nt)

  returnA  -< FacetDoc { facetDoc_id         = _node_id        doc
                       , facetDoc_created    = _node_date      doc
                       , facetDoc_title      = _node_name      doc
                       , facetDoc_hyperdata  = _node_hyperdata doc
                       , facetDoc_category   = toNullable $ sqlInt4 1
                       , facetDoc_ngramCount = toNullable $ sqlDouble 1.0
                       , facetDoc_score      = toNullable $ sqlDouble 1 }

queryAuthorsDoc :: Select ( NodeRead
                          , MaybeFields ContextNodeNgramsRead
                          , MaybeFields NgramsRead
                          , MaybeFields ContextNodeNgramsRead
                          , MaybeFields NodeRead)
queryAuthorsDoc = proc () -> do
  n <- queryNodeTable -< ()
  cnn <- optionalRestrict queryContextNodeNgramsTable -<
    \cnn' -> _node_id n .== _cnng_context_id cnn'
  ng <- optionalRestrict queryNgramsTable -<
    \ng' -> justFields (ng' ^. ngrams_id) .=== (_cnng_ngrams_id <$> cnn)
  cnn2 <- optionalRestrict queryContextNodeNgramsTable -<
    \cnn2' -> (_ngrams_id <$> ng) .=== justFields (_cnng_ngrams_id cnn2')
  contact <- optionalRestrict queryNodeTable -<
    \contact' -> justFields (_node_id contact') .=== (_cnng_context_id <$> cnn2)

  returnA -< (n, cnn, ng, cnn2, contact)


------------------------------------------------------------------------
-- TODO-SECURITY check
runViewDocuments :: (HasDBid NodeType, HasNodeError err)
                 => CorpusId
                 -> IsTrash
                 -> Maybe Offset
                 -> Maybe Limit
                 -> Maybe OrderBy
                 -> Maybe Text
                 -> Maybe Text
                 -> Cmd err [FacetDoc]
runViewDocuments cId t o l order query year = do
    listId <- defaultList cId

    res <- runOpaQuery $ filterWith' o l order (sqlQuery listId) :: Cmd err [FacetDocAgg']
    pure $ remapNgramsCount <$> res
  where
    sqlQuery lId = viewDocuments cId lId t (toDBid NodeDocument) query year

    remapNgramsCount (FacetDoc { .. }) =
      FacetDoc { facetDoc_ngramCount = Just $ fromIntegral facetDoc_ngramCount
               , facetDoc_score      = Just $ fromIntegral facetDoc_score
               , .. }

runCountDocuments :: (HasDBid NodeType, HasNodeError err)
                  => CorpusId -> IsTrash -> Maybe Text -> Maybe Text -> Cmd err Int
runCountDocuments cId t mQuery mYear = do
  listId <- defaultList cId
  runCountOpaQuery (sqlQuery listId)
  where
    sqlQuery lId = viewDocuments cId lId t (toDBid NodeDocument) mQuery mYear

viewDocuments :: CorpusId
               -> ListId
               -> IsTrash
               -> NodeTypeId
               -> Maybe Text
               -> Maybe Text
               -> Select FacetDocAgg
viewDocuments cId lId t ntId mQuery mYear =
  aggregate (pFacetDoc FacetDoc { facetDoc_id         = OAgg.groupBy
                                , facetDoc_created    = OAgg.groupBy
                                , facetDoc_title      = OAgg.groupBy
                                , facetDoc_hyperdata  = OAgg.groupBy
                                , facetDoc_category   = OAgg.groupBy
                                , facetDoc_ngramCount = OAgg.sumInt4
                                , facetDoc_score      = OAgg.groupBy })
        (viewDocumentsAgg cId lId t ntId mQuery mYear)

viewDocumentsAgg :: CorpusId
                 -> ListId
                 -> IsTrash
                 -> NodeTypeId
                 -> Maybe Text
                 -> Maybe Text
                 -> Select FacetDocAggPart
viewDocumentsAgg cId lId t ntId mQuery mYear = proc () -> do
  (c, nc) <- viewDocumentsQuery cId t ntId mQuery mYear -< ()
  cnng <- optionalRestrict queryContextNodeNgramsTable -<
    \cnng' -> (cnng' ^. cnng_node_id)    .== pgNodeId lId .&&  -- (nc ^. nc_node_id) .&&
              (cnng' ^. cnng_context_id) .== (c ^. cs_id)
  let ngramCount = fromMaybeFields 0 $ _cnng_doc_count <$> cnng
  returnA  -< FacetDoc { facetDoc_id         = _cs_id        c
                       , facetDoc_created    = _cs_date      c
                       , facetDoc_title      = _cs_name      c
                       , facetDoc_hyperdata  = _cs_hyperdata c
                       , facetDoc_category   = nc ^. nc_category
                       , facetDoc_ngramCount = ngramCount
                       -- NOTE This is a slight abuse of "score" but
                       -- currently it is all 0's in the DB and the
                       -- search functionality on the frontend orders
                       -- by Score.
                       , facetDoc_score      = unsafeCast "int8" $ nc ^. nc_score
                       }

-- TODO Join with context_node_ngrams at context_id/node_id and sum by
-- doc_count.
viewDocumentsQuery :: CorpusId
                   -> IsTrash
                   -> NodeTypeId
                   -> Maybe Text
                   -> Maybe Text
                   -> Select (ContextSearchRead, NodeContextRead)
viewDocumentsQuery cId t ntId mQuery mYear = proc () -> do
  c  <- queryContextSearchTable -< ()
  nc <- queryNodeContextTable -< ()
  restrict -< (c^.cs_id)       .== (nc^.nc_context_id)
  restrict -< nc^.nc_node_id   .== pgNodeId cId
  restrict -< c^.cs_typename   .== sqlInt4 ntId
  restrict -< if t then nc^.nc_category .== sqlInt4 0
                   else nc^.nc_category .>= sqlInt4 1

  let
    query         = (fromMaybe "" mQuery)
    year          = (fromMaybe "" mYear)
    iLikeQuery    = T.intercalate "" ["%", query, "%"]
    abstractLHS h = fromNullable (sqlStrictText "")
                  $ toNullable h .->> sqlStrictText "abstract"
    yearLHS h     = fromNullable (sqlStrictText "")
                  $ toNullable h .->> sqlStrictText "publication_year"

  restrict -<
    if query == "" then sqlBool True
      else  ((c^.cs_name) `ilike` (sqlStrictText iLikeQuery))
        .|| ((abstractLHS (c^.cs_hyperdata)) `ilike` (sqlStrictText iLikeQuery))
  restrict -<
    if year == "" then sqlBool True
      else yearLHS (c ^. cs_hyperdata) .== sqlStrictText year

  returnA -< (c, nc)
  -- returnA -< (c, nc, cnng)


------------------------------------------------------------------------
filterWith :: (SqlOrd date, SqlOrd title, SqlOrd category, SqlOrd score, hyperdata ~ SqlJsonb) =>
        Maybe Offset
     -> Maybe Limit
     -> Maybe OrderBy
     -> Select (Facet id (Field date) (Field title) (Field hyperdata) (FieldNullable category) ngramCount (FieldNullable score))
     -> Select (Facet id (Field date) (Field title) (Field hyperdata)(FieldNullable category) ngramCount (FieldNullable score))
filterWith o l order q = limit' l $ offset' o $ orderBy (orderWith order) q


orderWith :: (SqlOrd b1, SqlOrd b2, SqlOrd b3, SqlOrd b4)
          => Maybe OrderBy
          -> Order (Facet id (Field b1) (Field b2) (Field SqlJsonb) (FieldNullable b3) ngramCount (FieldNullable b4))
orderWith (Just DateAsc)   = asc  facetDoc_created
orderWith (Just DateDesc)  = desc facetDoc_created

orderWith (Just TitleAsc)  = asc  facetDoc_title
orderWith (Just TitleDesc) = desc facetDoc_title

orderWith (Just ScoreAsc)  = ascNullsLast  facetDoc_score
orderWith (Just ScoreDesc) = descNullsLast facetDoc_score

orderWith (Just SourceAsc)  = ascNullsLast  facetDoc_source
orderWith (Just SourceDesc) = descNullsLast facetDoc_source

orderWith (Just TagAsc)     = ascNullsLast  facetDoc_category
orderWith (Just TagDesc)    = descNullsLast facetDoc_category

orderWith _                = asc facetDoc_created



filterWith' :: (SqlOrd date, SqlOrd title, SqlOrd category, SqlOrd score, hyperdata ~ SqlJsonb, SqlOrd ngramCount) =>
        Maybe Offset
     -> Maybe Limit
     -> Maybe OrderBy
     -> Select (Facet id (Field date) (Field title) (Field hyperdata) (Field category) (Field ngramCount) (Field score))
     -> Select (Facet id (Field date) (Field title) (Field hyperdata) (Field category) (Field ngramCount) (Field score))
filterWith' o l order q = limit' l $ offset' o $ orderBy (orderWith' order) q


orderWith' :: (SqlOrd date, SqlOrd title, SqlOrd category, SqlOrd ngramCount, SqlOrd score)
           => Maybe OrderBy
           -> Order (Facet id (Field date) (Field title) (Field SqlJsonb) (Field category) (Field ngramCount) (Field score))
orderWith' (Just DateAsc)   = asc  facetDoc_created
orderWith' (Just DateDesc)  = desc facetDoc_created

orderWith' (Just TitleAsc)  = asc  facetDoc_title
orderWith' (Just TitleDesc) = desc facetDoc_title

orderWith' (Just NgramCountAsc)  = asc  facetDoc_ngramCount
orderWith' (Just NgramCountDesc) = desc facetDoc_ngramCount

orderWith' (Just ScoreAsc)  = asc  facetDoc_score
orderWith' (Just ScoreDesc) = desc facetDoc_score

orderWith' (Just SourceAsc)  = ascNullsLast  facetDoc_source
orderWith' (Just SourceDesc) = descNullsLast facetDoc_source

orderWith' (Just TagAsc)     = asc  facetDoc_category
orderWith' (Just TagDesc)    = desc facetDoc_category

orderWith' _                = asc facetDoc_created

facetDoc_source :: SqlIsJson a
                => Facet id created title (Field a) favorite ngramCount score
                -> FieldNullable SqlText
facetDoc_source x = (toNullable $ facetDoc_hyperdata x) .->> sqlString "source"
