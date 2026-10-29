# This script identifies the top 10 datasets (datasetKey) with the largest number of records that dropped from MIDS level 1 to MIDS level 0 between:

# 2018 → 2019
# 2020 → 2021


library(arrow)
library(dplyr)
library(stringr)

#==================================================
# Locate parquet files
#==================================================

files <- list.files(
  "outputs",
  pattern = "\\.parquet$",
  full.names = TRUE
)

#==================================================
# Extract years
#==================================================

years <- as.integer(
  str_extract(
    basename(files),
    "^\\d{4}"
  )
)

#==================================================
# Function
#==================================================

find_top_1_to_0 <- function(year_from, year_to) {
  
  cat(
    "\nProcessing ",
    year_from,
    " -> ",
    year_to,
    "\n"
  )
  
  file_from <- files[years == year_from]
  file_to   <- files[years == year_to]
  
  df_from <- open_dataset(file_from) |>
    select(
      gbifID,
      datasetKey,
      MIDS_level
    ) |>
    collect() |>
    rename(
      MIDS_old = MIDS_level
    )
  
  df_to <- open_dataset(file_to) |>
    select(
      gbifID,
      MIDS_level
    ) |>
    collect() |>
    rename(
      MIDS_new = MIDS_level
    )
  
  result <- inner_join(
    df_from,
    df_to,
    by = "gbifID"
  ) |>
    
    filter(
      MIDS_old == 1,
      MIDS_new == 0
    ) |>
    
    count(
      datasetKey,
      name = "records_1_to_0"
    )
  
  # Calculate total number of 1->0 transitions
  total_changes <- sum(result$records_1_to_0)
  
  # Add percentage contribution
  result <- result |>
    
    mutate(
      pct_of_all_1_to_0 =
        100 * records_1_to_0 / total_changes
    ) |>
    
    arrange(
      desc(records_1_to_0)
    ) |>
    
    slice_head(n = 10)  
  result
  
}

#==================================================
# 2018 -> 2019
#==================================================

top_2018_2019 <- find_top_1_to_0(
  2018,
  2019
)

print(top_2018_2019)

#==================================================
# 2020 -> 2021
#==================================================

top_2020_2021 <- find_top_1_to_0(
  2020,
  2021
)

print(top_2020_2021)

#==================================================
# Save results
#==================================================

write.csv(
  top_2018_2019,
  "analyses/Analysis6/Top10_MIDS1_to_0_2018_2019.csv",
  row.names = FALSE
)

write.csv(
  top_2020_2021,
  "analyses/Analysis6/Top10_MIDS1_to_0_2020_2021.csv",
  row.names = FALSE
)