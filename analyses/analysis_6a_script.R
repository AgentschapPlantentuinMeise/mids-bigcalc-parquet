# Here's an efficient, consolidated version that:

# Reads only gbifID and MIDS_level.
# Processes one year pair at a time.
# Calculates:
# transition counts (n_records)
# % of all matched records
# % of all changing records
# Classifies transitions as Increase/Decrease.
# Orders transitions logically.
# Outputs:
# Full transition table.
# Largest transition per year.
# Increase vs decrease summary.
# A faceted plot with counts as bars and both percentages as labels.

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
# Calculate transitions
#==================================================

transition_summary <- map_dfr(

  seq_len(length(files) - 1),

  function(i){

    year1 <- years[i]
    year2 <- years[i + 1]

    cat(
      "Processing",
      year1,
      "->",
      year2,
      "\n"
    )

    old <- open_dataset(files[i]) |>
      select(
        gbifID,
        MIDS_level
      ) |>
      collect()

    new <- open_dataset(files[i + 1]) |>
      select(
        gbifID,
        MIDS_level
      ) |>
      collect()

    joined <- inner_join(
      old,
      new,
      by = "gbifID",
      suffix = c("_old", "_new")
    )

    total_records <- nrow(joined)

    joined |>

      filter(
        MIDS_level_old != MIDS_level_new
      ) |>

      mutate(

        transition = paste0(
          "MIDS ",
          MIDS_level_old,
          " → MIDS ",
          MIDS_level_new
        ),

        interval = paste0(
          year1,
          "-",
          year2
        ),

        direction = if_else(
          MIDS_level_new > MIDS_level_old,
          "Increase",
          "Decrease"
        )

      ) |>

      count(
        interval,
        transition,
        direction,
        name = "n_records"
      ) |>

      mutate(
        total_records = total_records
      ) |>

      mutate(
        pct_of_all_records =
          100 * n_records / total_records
      ) |>

      mutate(
        pct_of_changes =
          100 * n_records / sum(n_records)
      )

  }

)

#==================================================
# Logical transition order
#==================================================

transition_levels <- c(
  
  # improvements
  
  "MIDS -1 → MIDS 0",
  "MIDS -1 → MIDS 1",
  "MIDS -1 → MIDS 2",
  "MIDS -1 → MIDS 3",
  
  "MIDS 0 → MIDS 1",
  "MIDS 0 → MIDS 2",
  "MIDS 0 → MIDS 3",
  
  "MIDS 1 → MIDS 2",
  "MIDS 1 → MIDS 3",
  
  "MIDS 2 → MIDS 3",
  
  # declines
  
  "MIDS 3 → MIDS 2",
  
  "MIDS 3 → MIDS 1",
  "MIDS 2 → MIDS 1",
  
  "MIDS 3 → MIDS 0",
  "MIDS 2 → MIDS 0",
  "MIDS 1 → MIDS 0",
  
  "MIDS 3 → MIDS -1",
  "MIDS 2 → MIDS -1",
  "MIDS 1 → MIDS -1",
  "MIDS 0 → MIDS -1"
  
)

transition_summary <- transition_summary |>
  
  mutate(
    
    transition = factor(
      transition,
      levels = rev(transition_levels)
    ),
    
    label = paste0(
      round(pct_of_changes, 1),
      "%"
    )
    
  )

#==================================================
# Save full transition table
#==================================================

write.csv(
  transition_summary,
  "analyses/Analysis6/MIDS_Level_Transitions_All_Years.csv",
  row.names = FALSE
)

#==================================================
# Largest transition per year
#==================================================

largest_transition <- transition_summary |>

  group_by(interval) |>

  slice_max(
    n_records,
    n = 1,
    with_ties = FALSE
  ) |>

  ungroup()

write.csv(
  largest_transition,
  "analyses/Analysis6/Largest_MIDS_Transition_Per_Year.csv",
  row.names = FALSE
)

#==================================================
# Increase vs decrease summary
#==================================================

direction_summary <- transition_summary |>

  group_by(
    direction,
    interval
      ) |>

  summarise(
    n_records = sum(n_records),
    pct_of_all_records =
      sum(pct_of_all_records),
    pct_of_changes =
      sum(pct_of_changes),
    .groups = "drop"
  )

write.csv(
  direction_summary,
  "analyses/Analysis6/Increase_Decrease_Summary.csv",
  row.names = FALSE
)

#==================================================
# Plot
#==================================================

p <- ggplot(
  transition_summary,
  aes(
    x = n_records,
    y = transition,
    fill = direction
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(label = label),
    hjust = -0.15,
    size = 3
  ) +
  
  facet_wrap(
    ~ interval,
    scales = "free_x",
    ncol = 3
  ) +
  
  scale_fill_manual(
    values = c(
      "Decrease" = "#d73027",
      "Increase" = "#1a9850"
    )
  ) +
  
  scale_x_continuous(
    labels = comma,
    expand = expansion(
      mult = c(0, 0.15)
    )
  ) +
  
  labs(
    title = "Changes in MIDS Level Between Consecutive Snapshots",
    subtitle = "Labels show the percentage of all changing records represented by each transition",
    x = "Number of Specimens",
    y = "Transition",
    fill = ""
  ) +
  
  theme_bw() +
  
  theme(
    strip.text = element_text(
      face = "bold"
    ),
    legend.position = "bottom"
  )

print(p)

#==================================================
# Save figure
#==================================================

ggsave(
  "analyses/Analysis6/MIDS_Level_Transitions_All_Years.png",
  plot = p,
  width = 16,
  height = 11,
  dpi = 300
)