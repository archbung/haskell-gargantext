
\COPY (select count(*), date_trunc('month', n.date) from nodes n where n.typename = 210 group by 2 ORDER BY 2) TO '/tmp/teams.csv' (FORMAT csv);






