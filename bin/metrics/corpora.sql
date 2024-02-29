WITH total AS (SELECT * from nodes n where n.typename = 30)
    , sum AS (SELECT count(*) from total)
    , increase AS (SELECT count(*) from total as t WHERE t.date >= date_trunc('month', current_date - interval '3' month))
SELECT *,
  (SELECT (100 * (SELECT * from increase) / (SELECT * from sum))) AS "LAST 3 MONTHS / TOTAL"
     FROM sum




