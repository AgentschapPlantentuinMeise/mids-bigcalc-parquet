duckdb -csv -noheader -c  "select count(*) from read_parquet('occurrence_parquet/*/*.parquet', hive_partitioning=true);" > count.txt
duckdb -csv -noheader -c  "select count(*) from read_csv('occurence_rejects.csv');" > rejects.txt
