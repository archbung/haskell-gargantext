-- Remove unused old materialized view
drop materialized view context_node_ngrams_view;

-- FIX NGRAMS Parents
with query as (
with child_ngrams as
(select jsonb_array_elements_text(ngrams_repo_element->'children') as term
from node_stories),
parent_ngrams as
(select ngrams_repo_element->>'root' as term
from node_stories)

(select child_ngrams.term, ngrams.terms
from child_ngrams
left join ngrams on child_ngrams.term = ngrams.terms
where ngrams.terms is null

union

select parent_ngrams.term, ngrams.terms
from parent_ngrams
left join ngrams on parent_ngrams.term = ngrams.terms
where ngrams.terms is null
and parent_ngrams.term is not null)

order by term
)
INSERT INTO ngrams (terms) select term from query;


-- ADD triggers
 CREATE OR REPLACE FUNCTION check_node_stories_json()
  RETURNS TRIGGER AS $$
  DECLARE
    missing_ngrams_exist boolean;
  BEGIN
      WITH child_ngrams as
        (SELECT jsonb_array_elements_text(NEW.ngrams_repo_element->'children') AS term),
        parent_ngrams AS
        (SELECT NEW.ngrams_repo_element->>'root' AS term),

        ngrams_child_parent AS
            (SELECT child_ngrams.term, ngrams.terms
         FROM child_ngrams
         LEFT JOIN ngrams ON child_ngrams.term = ngrams.terms
         WHERE ngrams.terms IS NULL

        UNION

        SELECT parent_ngrams.term, ngrams.terms
        FROM parent_ngrams
        LEFT JOIN ngrams ON parent_ngrams.term = ngrams.terms
        WHERE ngrams.terms IS NULL
        AND parent_ngrams.term IS NOT NULL)

            SELECT EXISTS(SELECT * FROM ngrams_child_parent) INTO missing_ngrams_exist;

            IF missing_ngrams_exist THEN
              RAISE EXCEPTION 'node_stories: ngrams are missing: %', row_to_json(NEW);
        END IF;

            RETURN NEW;
  END;
  $$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER check_node_stories_json_trg
AFTER INSERT OR UPDATE
ON node_stories
FOR EACH ROW
EXECUTE PROCEDURE check_node_stories_json();


   CREATE OR REPLACE FUNCTION check_ngrams_json()
    RETURNS TRIGGER AS $$
    DECLARE
      missing_ngrams_exist boolean;
    BEGIN
        WITH child_ngrams as
          (SELECT jsonb_array_elements_text(ngrams_repo_element->'children') AS term
           FROM node_stories
           WHERE term = OLD.terms),
          parent_ngrams AS
          (SELECT ngrams_repo_element->>'root' AS term
           FROM node_stories
           WHERE term = OLD.terms),
          child_parent_ngrams AS
          (SELECT * FROM child_ngrams
           UNION SELECT * FROM parent_ngrams)

       SELECT EXISTS(SELECT * FROM child_parent_ngrams) INTO missing_ngrams_exist;

       IF missing_ngrams_exist THEN
          RAISE EXCEPTION 'ngrams are missing: %', row_to_json(OLD);
       END IF;

       RETURN OLD;
    END;
    $$ LANGUAGE plpgsql;

  CREATE OR REPLACE TRIGGER check_ngrams_json_trg
  AFTER DELETE
  ON ngrams
  FOR EACH ROW
  EXECUTE PROCEDURE check_ngrams_json();




