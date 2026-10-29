duckdb -c "COPY (SELECT * FROM read_csv('multimedia.txt', delim='\t', quote='', all_varchar=True))
        TO 'multimedia_parquet'
        (FORMAT 'PARQUET', COMPRESSION 'ZSTD');"
