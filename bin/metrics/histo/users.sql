
\COPY (select count(*), date_trunc('month', a.date_joined) from auth_user a group by 2) TO '/tmp/users.csv' (FORMAT csv);





