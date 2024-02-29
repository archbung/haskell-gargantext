WITH total AS (SELECT * from auth_user as A)
    , sum AS (SELECT count(*) from total)
    , increase AS (SELECT count(*) from total as t WHERE t.date_joined >= date_trunc('month', current_date - interval '3' month))
SELECT *,
  (SELECT (100 * (SELECT * from increase) / (SELECT * from sum))) AS "LAST 3 MONTHS / TOTAL"
     FROM sum




