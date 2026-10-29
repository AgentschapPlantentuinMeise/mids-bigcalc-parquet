library(arrow)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(scales)

# List parquet files
files <- list.files(
  path = "outputs",
  pattern = "\\.parquet$",
  full.names = TRUE
)

# Extract years from filenames
years <- as.integer(
  stringr::str_extract(basename(files), "^\\d{4}")
)

# Ensure files are in year order
ord <- order(years)

files <- files[ord]
years <- years[ord]

# Calculate transition counts
transition_counts <- map_dfr(
  seq_len(length(files) - 1),
  function(i) {
    
    year1 <- years[i]
    year2 <- years[i + 1]
    
    cat("Processing", year1, "to", year2, "\n")
    
    df1 <- open_dataset(files[i]) |>
      select(gbifID, MIDS_level) |>
      collect() |>
      rename(MIDS_from = MIDS_level)
    
    df2 <- open_dataset(files[i + 1]) |>
      select(gbifID, MIDS_level) |>
      collect() |>
      rename(MIDS_to = MIDS_level)
    
    inner_join(df1, df2, by = "gbifID") |>
      mutate(
        transition = paste0(
          "MIDS ", MIDS_from,
          " → MIDS ", MIDS_to
        ),
        status = if_else(
          MIDS_from == MIDS_to,
          "Unchanged",
          "Changed"
        ),
        interval = paste0(year1, "-", year2)
      ) |>
      count(interval, status)
    
  }
)

# Summarise change vs no change
status_summary <- transition_counts |>
  group_by(interval) |>
  mutate(
    total = sum(n),
    percent = n / total
  ) |>
  ungroup()

# Inspect results
print(status_summary)

ggplot(
  status_summary,
  aes(
    x = interval,
    y = percent,
    fill = status
  )
) +
  geom_col() +
  scale_y_continuous(
    labels = percent_format()
  ) +
  labs(
    title = "Percentage of Specimen Records Changing MIDS Level",
    x = "Year Interval",
    y = "Percentage of Records",
    fill = ""
  ) +
  theme_bw()


ggplot(
  status_summary,
  aes(
    x = interval,
    y = n,
    fill = status
  )
) +
  geom_col() +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Number of Specimen Records Changing MIDS Level",
    x = "Year Interval",
    y = "Number of Records",
    fill = ""
  ) +
  theme_bw()

