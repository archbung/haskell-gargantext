select count(*), date_trunc('month', n.date) from nodes n where n.typename = 90 group by 2 ORDER BY 2;

