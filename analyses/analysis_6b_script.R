# The script below identifies records whose MIDS level decreased between consecutive years, then summarizes the institutions responsible, ordered by the number of specimens with a reduction.

library(arrow)
library(dplyr)
library(purrr)
library(stringr)

# ----------------------------------------------------------
# Locate files
# ----------------------------------------------------------

parquet_dir <- "outputs"

files <- list.files(
  parquet_dir,
  pattern = "\\.parquet$",
  full.names = TRUE
)

years <- as.integer(
  str_extract(
    basename(files),
    "^\\d{4}"
  )
)

ord <- order(years)

files <- files[ord]
years <- years[ord]

# ----------------------------------------------------------
# Process one year pair at a time
# ----------------------------------------------------------

decline_list <- vector(
  "list",
  length(years) - 1
)

for(i in seq_len(length(years) - 1)) {
  
  year1 <- years[i]
  year2 <- years[i + 1]
  
  cat(
    "\nProcessing ",
    year1,
    " -> ",
    year2,
    "\n"
  )
  
  old <- open_dataset(files[i]) |>
    select(
      gbifID,
      datasetKey,
      MIDS_level
    ) |>
    collect() |>
    rename(
      MIDS_old = MIDS_level
    )
  
  new <- open_dataset(files[i + 1]) |>
    select(
      gbifID,
      MIDS_level
    ) |>
    collect() |>
    rename(
      MIDS_new = MIDS_level
    )
  
  decline_list[[i]] <- inner_join(
    old,
    new,
    by = "gbifID"
  ) |>
    
    mutate(
      change = MIDS_new - MIDS_old
    ) |>
    
    filter(
      change < 0
    ) |>
    
    mutate(
      from_year = year1,
      to_year = year2
    )
  
  rm(old, new)
  
  gc()
  
}

declines <- bind_rows(
  decline_list
)

rm(decline_list)

gc()

# ----------------------------------------------------------
# Rank datasets by declining records
# ----------------------------------------------------------

datasetKey_summary <- declines |>
  
  count(
    datasetKey,
    name = "n_declined_specimens"
  ) |>
  
  arrange(
    desc(n_declined_specimens)
  )

write.csv(
  datasetKey_summary,
  "analyses/Analysis6/datasetKeys_with_mids_declines.csv",
  row.names = FALSE
)

# ----------------------------------------------------------
# Breakdown by transition
# ----------------------------------------------------------

datasetKey_transition_summary <- declines |>
  
  count(
    from_year,
    to_year,
    datasetKey,
    name = "n_declined_specimens"
  ) |>
  
  arrange(
    from_year,
    desc(n_declined_specimens)
  )

write.csv(
  datasetKey_transition_summary,
  "analyses/Analysis6/datasetKeys_with_mids_declines_by_transition.csv",
  row.names = FALSE
)

# ----------------------------------------------------------
# Severity
# ----------------------------------------------------------

datasetKey_severity <- declines |>
  
  group_by(datasetKey) |>
  
  summarise(
    specimens_declined = n(),
    mean_drop = mean(abs(change)),
    max_drop = max(abs(change)),
    .groups = "drop"
  ) |>
  
  arrange(
    desc(specimens_declined)
  )

write.csv(
  datasetKey_severity,
  "analyses/Analysis6/datasetKeys_decline_severity.csv",
  row.names = FALSE
)