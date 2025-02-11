{-|
Module      : Gargantext.Database.Triggers.NodeNodeNgrams
Description : Triggers configuration
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Triggers on NodeNodeNgrams table.

-}

{-# LANGUAGE QuasiQuotes       #-}

module Gargantext.Database.Admin.Trigger.ContextNodeNgrams
  where

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Core
-- import Gargantext.Core.Types.Main (ListType(CandidateTerm))
import Gargantext.Database.Admin.Types.Node -- (ListId, CorpusId, NodeId)
-- (ListId, CorpusId, NodeId)
import Gargantext.Database.Prelude (execPGSQuery, DBCmd)
import Gargantext.Prelude
import qualified Database.PostgreSQL.Simple as DPS

triggerCountInsert :: HasDBid NodeType => DBCmd err Int64
triggerCountInsert = execPGSQuery query (toDBid NodeDocument, toDBid NodeList)
  where
    query :: DPS.Query
    query = [sql|
          CREATE OR REPLACE FUNCTION set_ngrams_global_count() RETURNS trigger AS $$
          BEGIN
            IF pg_trigger_depth() <> 1 THEN
              RETURN NEW;
            END IF;
            IF TG_OP = 'INSERT' THEN
                INSERT INTO node_node_ngrams (node1_id, node2_id, ngrams_id, ngrams_type, weight)
                select n.parent_id, n.id, new0.ngrams_id, new0.ngrams_type, count(*) from NEW as new0
                    INNER JOIN contexts n ON n.id  = new0.context_id
                    INNER JOIN nodes n2 ON n2.id = new0.node_id
                    WHERE n2.typename = ?  -- not mandatory
                      AND n.typename = ?   -- not mandatory
                      AND n.parent_id <> n2.id -- not mandatory
                    GROUP BY n.parent_id, n.id, new0.ngrams_id, new0.ngrams_type
                ON CONFLICT (node1_id, node2_id, ngrams_id, ngrams_type)
                   DO UPDATE set weight = node_node_ngrams.weight + excluded.weight
                   ;
            END IF;

            RETURN NULL;
          END
          $$ LANGUAGE plpgsql;

          DROP trigger IF EXISTS trigger_count_insert on context_node_ngrams;

          CREATE TRIGGER trigger_count_insert AFTER INSERT on context_node_ngrams
          REFERENCING NEW TABLE AS NEW
          FOR EACH STATEMENT
          EXECUTE PROCEDURE set_ngrams_global_count();
   |]

triggerCountInsert2 :: HasDBid NodeType => DBCmd err Int64
triggerCountInsert2 = execPGSQuery query ( toDBid NodeCorpus
                                         , toDBid NodeDocument
                                         , toDBid NodeList
                                         )
  where
    query :: DPS.Query
    query = [sql|
          CREATE OR REPLACE FUNCTION set_ngrams_global_count2() RETURNS trigger AS $$
          BEGIN
            IF pg_trigger_depth() <> 1 THEN
              RETURN NEW;
            END IF;
            IF TG_OP = 'INSERT' THEN
                INSERT INTO context_node_ngrams2 (context_id, nodengrams_id, weight)
                  SELECT corpus.id, nng.id, count(*) from NEW as new3
                        INNER JOIN node_ngrams nng    ON nng.id      = new3.nodengrams_id
                        INNER JOIN nodes       list   ON list.id     = nng.node_id
                        INNER JOIN nodes_nodes nn     ON nn.node2_id = new3.context_id
                        INNER JOIN nodes       corpus ON corpus.id   = nn.node1_id
                        INNER JOIN nodes       doc    ON doc.id      = nn.node2_id
                        WHERE corpus.typename = ? -- 30 -- corpus
                          AND doc.typename    = ? -- 4  -- maybe not mandatory
                          AND list.typename   = ? -- 5  -- list
                        GROUP BY corpus.id, nng.id

                ON CONFLICT (context_id, nodengrams_id)
                   DO UPDATE set weight = context_node_ngrams2.weight + excluded.weight
                   ;
            END IF;

            RETURN NULL;
          END
          $$ LANGUAGE plpgsql;

          DROP trigger IF EXISTS trigger_count_insert2 on context_node_ngrams2;

          CREATE TRIGGER trigger_count_insert2 AFTER INSERT on context_node_ngrams2
          REFERENCING NEW TABLE AS NEW
          FOR EACH STATEMENT
          EXECUTE PROCEDURE set_ngrams_global_count2();
   |]

-- TODO add the groups
-- TODO use context instead of nodes of type doc
{-
triggerCoocInsert :: HasDBid NodeType => Cmd err Int64
triggerCoocInsert = execPGSQuery query ( toDBid NodeCorpus
                                       , toDBid NodeDocument
                                       , toDBid NodeList
                                       , toDBid CandidateTerm
                                       , toDBid CandidateTerm
                                       )
  where
    query :: DPS.Query
    query = [sql|
          CREATE OR REPLACE FUNCTION set_cooc() RETURNS trigger AS $$
          BEGIN
            IF pg_trigger_depth() <> 1 THEN
              RETURN NEW;
            END IF;
            IF TG_OP = 'INSERT' THEN
                INSERT INTO node_nodengrams_nodengrams (node_id, node_ngrams1_id, node_ngrams2_id, weight)
                WITH input(corpus_id, nn1, nn2, weight) AS (
                  SELECT corpus.id, nng1.id, nng2.id, count(*) from NEW as new2
                        INNER JOIN node_ngrams nng1   ON nng1.id     = new2.nodengrams_id
                        INNER JOIN nodes       list   ON list.id     = nng1.node_id
                        INNER JOIN nodes_nodes nn     ON nn.node2_id = new2.node_id
                        INNER JOIN nodes       corpus ON corpus.id   = nn.node1_id
                        INNER JOIN nodes       doc    ON doc.id      = nn.node2_id

                        INNER JOIN node_node_ngrams2 nnng2 ON nnng2.node_id  = doc.id
                        INNER JOIN node_ngrams       nng2  ON nng2.id        = nnng2.nodengrams_id

                        WHERE corpus.typename = ? -- 30 -- corpus
                          AND doc.typename    = ? -- 4  -- maybe not mandatory
                          AND list.typename   = ? -- 5  -- list
                          AND nng2.node_id    = list.id
                          AND nng1.id < nng2.id
                          AND nng1.node_subtype >= ?
                          AND nng2.node_subtype >= ?
                        GROUP BY corpus.id, nng1.id, nng2.id
                        )
                    SELECT * from input where weight > 1

                ON CONFLICT (node_id, node_ngrams1_id, node_ngrams2_id)
                   DO UPDATE set weight = node_nodengrams_nodengrams.weight + excluded.weight
                   ;
            END IF;

            RETURN NULL;
          END
          $$ LANGUAGE plpgsql;

          -- DROP trigger trigger_cooc on node_node_ngrams2;

          CREATE TRIGGER trigger_cooc_insert AFTER INSERT on node_node_ngrams2
          REFERENCING NEW TABLE AS NEW
          FOR EACH STATEMENT
          EXECUTE PROCEDURE set_cooc();
   |]
-}
