# Identify all records present in the 2016 snapshot.
# For each later snapshot (2017-2026), find those same records using gbifID.
# Calculate the distribution of MIDS levels among only those original 2016 records.
# Visualise how that cohort migrates from MIDS 0 → 1 → 2 → 3 through time.

# This removes the influence of newly-added records and focuses entirely on the enrichment of the original 2016 records.

library(arrow)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(scales)

#==================================================
# Locate parquet files
#==================================================

files <- list.files(
  path = "outputs",
  pattern = "\\.parquet$",
  full.names = TRUE
)

#==================================================
# Extract years and sort
#==================================================

years <- as.integer(
  str_extract(
    basename(files),
    "^\\d{4}"
  )
)

ord <- order(years)

files <- files[ord]
years <- years[ord]

#==================================================
# Identify 2016 file
#==================================================

file_2016 <- files[
  years == 2016
]

#==================================================
# Get 2016 cohort
#==================================================

records_2016 <- open_dataset(file_2016) |>
  select(gbifID) |>
  collect()

cat(
  "Records present in 2016:",
  nrow(records_2016),
  "\n"
)

#==================================================
# Track 2016 records through time
#==================================================

cohort_summary <- map_dfr(

  seq_along(files),

  function(i){

    year <- years[i]

    cat(
      "Processing",
      year,
      "\n"
    )

    current <- open_dataset(files[i]) |>
      select(
        gbifID,
        MIDS_level
      ) |>
      collect()

    cohort <- inner_join(
      records_2016,
      current,
      by = "gbifID"
    )

    cohort |>
      count(MIDS_level) |>
      mutate(
        snapshot_year = year,
        total_records = sum(n),
        pct_records = n / sum(n)
      )

  }

)

#==================================================
# Save results
#==================================================

write.csv(
  cohort_summary,
  "MIDS_2016_Cohort_Through_Time.csv",
  row.names = FALSE
)

#==================================================
# Order MIDS levels
#==================================================

cohort_summary <- cohort_summary |>

  mutate(
    MIDS_level = factor(
      MIDS_level,
      levels = c(
        -1, 0, 1, 2, 3
      )
    )
  )

# Graph 1. Shows how the composition of the original 2016 records changes over time.

ggplot(
  cohort_summary,
  aes(
    x = snapshot_year,
    y = pct_records,
    fill = MIDS_level
  )
) +
  geom_col(
    position = "fill"
  ) +
  scale_y_continuous(
    labels = percent_format()
  ) +
  labs(
    title = "MIDS Progression of Records Present in 2016",
    subtitle = "Distribution of MIDS levels through time",
    x = "Year",
    y = "Proportion of Records",
    fill = "MIDS Level"
  ) +
  theme_bw()

# Graph 2 Absolute numbers.

ggplot(
  cohort_summary,
  aes(
    x = snapshot_year,
    y = n,
    fill = MIDS_level
  )
) +
  geom_col() +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "MIDS Levels of 2016 Records Through Time",
    x = "Year",
    y = "Number of Records",
    fill = "MIDS Level"
  ) +
  theme_bw()

# Graph 3 Line chart showing each MIDS level separately.

ggplot(
  cohort_summary,
  aes(
    x = snapshot_year,
    y = pct_records,
    colour = MIDS_level,
    group = MIDS_level
  )
) +
  geom_line(
    linewidth = 1.3
  ) +
  geom_point(
    size = 3
  ) +
  scale_y_continuous(
    labels = percent_format()
  ) +
  scale_x_continuous(
    breaks = sort(
      unique(cohort_summary$snapshot_year)
    )
  ) +
  labs(
    title = "Trajectory of 2016 Records Through MIDS Levels",
    x = "Year",
    y = "Percentage of Records",
    colour = "MIDS Level"
  ) +
  theme_bw()


# For each year

# Rather than tracking only the 2016 cohort, create a cohort for each intake year:

# New records added in 2017 (relative to 2016)
# New records added in 2018 (relative to 2017)
# ...
# New records added in 2025 (relative to 2024)

# Then follow each cohort forward through all subsequent years and observe how their MIDS distributions change.

# This gives a view of: Do records arriving in later years start at higher MIDS levels and/or undergo more rapid enrichment?

library(arrow)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(scales)

#==================================================
# Locate parquet files
#==================================================

files <- list.files(
  path = "outputs",
  pattern = "\\.parquet$",
  full.names = TRUE
)

#==================================================
# Extract years and sort
#==================================================

years <- as.integer(
  str_extract(
    basename(files),
    "^\\d{4}"
  )
)

ord <- order(years)

files <- files[ord]
years <- years[ord]

#==================================================
# Build cohorts
#==================================================

cohort_results <- map_dfr(

  2:length(files),

  function(i){

    cohort_year <- years[i]

    if(cohort_year > 2025) {
      return(NULL)
    }

    cat(
      "\nCreating cohort:",
      cohort_year,
      "\n"
    )

    previous <- open_dataset(files[i - 1]) |>
      select(gbifID) |>
      collect()

    current <- open_dataset(files[i]) |>
      select(
        gbifID,
        MIDS_level
      ) |>
      collect()

    #---------------------------------------
    # New records for this cohort
    #---------------------------------------

    cohort_ids <- anti_join(
      current,
      previous,
      by = "gbifID"
    ) |>
      select(gbifID)

    cat(
      "New records:",
      nrow(cohort_ids),
      "\n"
    )

    #---------------------------------------
    # Follow cohort through time
    #---------------------------------------

    map_dfr(

      i:length(files),

      function(j){

        tracking_year <- years[j]

        cat(
          "  Tracking in",
          tracking_year,
          "\n"
        )

        snapshot <- open_dataset(files[j]) |>
          select(
            gbifID,
            MIDS_level
          ) |>
          collect()

        cohort_snapshot <- inner_join(
          cohort_ids,
          snapshot,
          by = "gbifID"
        )

        cohort_snapshot |>
          count(MIDS_level) |>
          mutate(
            cohort_year = cohort_year,
            snapshot_year = tracking_year,
            total_records = sum(n),
            pct_records = n / sum(n)
          )

      }

    )

  }

)

#==================================================
# Save results
#==================================================

write.csv(
  cohort_results,
  "MIDS_Cohorts_2017_2025.csv",
  row.names = FALSE
)

#==================================================
# Order MIDS levels
#==================================================

cohort_results <- cohort_results |>
  mutate(
    MIDS_level = factor(
      MIDS_level,
      levels = c(-1, 0, 1, 2, 3)
    )
  )

#==================================================
#==================================================
# Visualisation 1 Each panel is a cohort.
#==================================================

ggplot(
  cohort_results,
  aes(
    snapshot_year,
    pct_records,
    fill = MIDS_level
  )
) +
  geom_col(position = "fill") +
  facet_wrap(
    ~ cohort_year,
    ncol = 3
  ) +
  scale_y_continuous(
    labels = percent_format()
  ) +
  labs(
    title = "MIDS Level Progression of New Record Cohorts",
    subtitle = "Each panel follows records first appearing in a given year",
    x = "Year",
    y = "Proportion of Records",
    fill = "MIDS Level"
  ) +
  theme_bw()

#==================================================
# Visualisation 3 Compare where cohorts entered the system.
#==================================================

entry_year <- cohort_results |>
  filter(
    snapshot_year == cohort_year
  )

ggplot(
  entry_year,
  aes(
    factor(cohort_year),
    pct_records,
    fill = MIDS_level
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(
    labels = percent_format()
  ) +
  labs(
    title = "MIDS Distribution at Time of First Appearance",
    subtitle = "How mature were records when they entered GBIF?",
    x = "Cohort Year",
    y = "Proportion of Records",
    fill = "MIDS Level"
  ) +
  theme_bw()

# This last figure is often particularly revealing because it shows whether records entering in later years are arriving already digitised at MIDS 2-3 rather than entering at MIDS 0-1 and being enriched later.