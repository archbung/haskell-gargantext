\COPY (SELECT count(*), date_trunc('month', n.date) FROM nodes n WHERE n.typename = 30 GROUP BY 2 ORDER BY 2) TO '/tmp/corpora.csv' (FORMAT csv);


