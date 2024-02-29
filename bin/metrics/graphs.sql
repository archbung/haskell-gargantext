WITH total AS (SELECT * from nodes n where n.typename = 9)
    , sum AS (SELECT count(*) AS "TOTAL" from total)
    , increase AS (SELECT count(*) from total as t WHERE t.date >= date_trunc('month', current_date - interval '3' month))
SELECT *,
  (SELECT TO_CHAR((ROUND(100 * NULLIF((SELECT * from increase),0) / NULLIF((SELECT * from sum), 0))), 'fm99%')  AS "CREATED LAST 3 MONTHS")
     FROM sum

