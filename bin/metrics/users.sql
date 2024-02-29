WITH total AS (SELECT * from auth_user as A)
    , sum AS (SELECT count(*) AS "TOTAL" from total)
    , increase AS (SELECT count(*) from total as t WHERE t.date_joined >= date_trunc('month', current_date - interval '3' month))
SELECT *,
  (SELECT TO_CHAR((ROUND(100 * (SELECT * from increase) / (SELECT * from sum))), 'fm99%')  AS "CREATED LAST 3 MONTHS")
     FROM sum




