WITH x_cte AS (
    SELECT * FROM t2
)
SELECT
    abc
FROM
    t1, x_cte
WHERE
    id = 123
GROUP BY
    abc
HAVING
    def > 1; /* this should be ignored /* nested comments **/ */