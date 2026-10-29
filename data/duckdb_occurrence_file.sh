duckdb -c "COPY (
            SELECT * FROM read_csv(
                'occurrence.txt',
                delim='\t',
                all_varchar=True,
                quote='',
                store_rejects=true,
                rejects_table='csv_rejects', strict_mode=False
            )
        )
        TO 'occurrence_parquet'
        (FORMAT 'PARQUET', COMPRESSION 'ZSTD', PARTITION_BY (basisofrecord)); copy csv_rejects to 'rejects.csv' (format 'CSV');"