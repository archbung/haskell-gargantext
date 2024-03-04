
\COPY (SELECT count(*), date_trunc('month', n.date) from nodes n where n.typename = 9 group by 2 ORDER BY 2) TO '/tmp/graphs.csv' (FORMAT csv);

