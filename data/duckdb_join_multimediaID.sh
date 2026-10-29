duckdb -c "COPY (
  SELECT
                            a.*,
                            b.multimediaidentifier
                        FROM 'specimen_2016-06-07_0004071-160526112335914.parquet' a
                        LEFT JOIN (
                            SELECT
                                gbifID,
                                CASE WHEN bool_or(identifier IS NULL) THEN NULL ELSE ANY_VALUE(identifier) END AS multimediaIdentifier
                            FROM 'multimedia_2016-06-07_0004071-160526112335914.parquet'
                            WHERE identifier IS NOT NULL
                            GROUP BY gbifID
                        ) b ON a.gbifID = b.gbifID
   ) TO 'specimulti_2016-06-07_0004071-160526112335914.parquet'
           (FORMAT 'PARQUET', COMPRESSION 'ZSTD');"
