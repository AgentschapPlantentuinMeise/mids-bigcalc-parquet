library(arrow)
library(dplyr)
library(duckdb)

# 1. Open datasets lazily in Arrow
big_df <- open_dataset("../0004071-160526112335914/preserved_specimen/data_0.parquet")
small_df <- open_dataset("../multimedia_2016-06-07_0004071-160526112335914.parquet")

# 2. Connect to DuckDB
con <- dbConnect(duckdb())

# 3. Enforce a strict RAM limit (e.g., 4GB or 8GB depending on your system)
dbExecute(con, "SET memory_limit = '28GB'")
dbExecute(con, "SET temp_directory = 'duckdb_temp'") # Spills excess data here

# 4. Register Arrow pointers (zero memory overhead)
duckdb_register_arrow(con, "big_table", big_df)
duckdb_register_arrow(con, "small_table", small_df %>% select(gbifID, identifier))

# don't run: will crash OOM

# 5. Stream the result directly to Parquet files
# dbExecute(con, "
#   COPY (
#     SELECT b.*, s.identifier AS mediaIdentifier
#     FROM big_table b
#     LEFT JOIN small_table s ON b.gbifID = s.gbifID
#   ) TO 'outputs/2016_joined.parquet'
#   (FORMAT PARQUET, ROW_GROUP_SIZE 80000);
# ")

# Clean up
dbDisconnect(con, shutdown = TRUE)