# A script that mirrors the continent and kingdom analyses, but groups specimens by collection year into:

# Pre-1800
# 1800-1899
# 1900-1949
# 1950-1999
# 2000-2024
# Unknown

# and then:

# Calculates MIDS element completeness for each group and each snapshot year.
# Produces a faceted plot with one panel per snapshot year.
# Shows grouped vertical bars for the year groups within each MIDS element.
# Uses consistent colours across all years.
# Saves the summary table and figure.

library(arrow)
library(bit64)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readr)
library(stringr)
library(purrr)

#==================================================
# Locate parquet files
#==================================================

files <- list.files(
  path = "outputs",
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

#==================================================
# Process all years
#==================================================

all_years_results <- map_dfr(

  seq_along(files),

  function(i){

    year <- years[i]

    cat(
      "Processing",
      year,
      "\n"
    )

    ds <- open_dataset(
      files[i]
    )

    bool_cols <- names(ds)[

      sapply(
        ds$schema$fields,
        function(f)
          f$type$ToString() == "bool"
      )

    ]

    #------------------------------------------
    # Load required columns only
    #------------------------------------------

    df <- ds |>
      select(
        year,
        all_of(bool_cols)
      ) |>
      collect()

    #------------------------------------------
    # Create year groups
    #------------------------------------------

    df <- df |>

      mutate(

        year_group = case_when(

          is.na(year) ~ "Unknown",

          year < 1800 ~ "Pre-1800",

          year >= 1800 &
            year <= 1899 ~ "1800-1899",

          year >= 1900 &
            year <= 1949 ~ "1900-1949",

          year >= 1950 &
            year <= 1999 ~ "1950-1999",

          year >= 2000 &
            year <= 2024 ~ "2000-2024",

          TRUE ~ "Unknown"

        )

      )

    #------------------------------------------
    # Summarise by year group
    #------------------------------------------

    year_results <- map_dfr(

      c(
        "Pre-1800",
        "1800-1899",
        "1900-1949",
        "1950-1999",
        "2000-2024",
        "Unknown"
      ),

      function(current_group){

        cat(
          "   ",
          current_group,
          "\n"
        )

        subset_df <- df |>
          filter(
            year_group == current_group
          )

        total_rows <- nrow(subset_df)

        if(total_rows == 0){
          return(NULL)
        }

        tibble(
          year_group = current_group,
          snapshot_year = year,
          column_name = bool_cols,
          true_count = sapply(
            subset_df[bool_cols],
            function(x)
              sum(
                as.integer(x),
                na.rm = TRUE
              )
          ),
          total_rows = total_rows
        )

      }

    )

    year_results |>

      mutate(

        true_percentage =
          100 * true_count /
          total_rows,

        column_name =
          gsub(
            "mids:MIDS",
            "",
            column_name,
            fixed = TRUE
          )

      )

  }

)

#==================================================
# Save results
#==================================================

write.csv(
  all_years_results,
  "analyses/Analysis10/YearGroup_MIDS_Completeness_All_Years.csv",
  row.names = FALSE
)

#==================================================
# Order factors
#==================================================

all_years_results <- all_years_results |>

  mutate(

    year_group = factor(
      year_group,
      levels = c(
        "Pre-1800",
        "1800-1899",
        "1900-1949",
        "1950-1999",
        "2000-2024",
        "Unknown"
      )
    )

  )

#==================================================
# Plot
# One panel per snapshot year
#==================================================

p <- ggplot(
  all_years_results,
  aes(
    x = column_name,
    y = true_percentage,
    fill = year_group
  )
) +

  geom_col(
    position = position_dodge(
      width = 0.8
    )
  ) +

  facet_wrap(
    ~ snapshot_year,
    ncol = 3
  ) +

  scale_fill_manual(
    values = c(
      "Pre-1800"   = "#1b9e77",
      "1800-1899"  = "#d95f02",
      "1900-1949"  = "#7570b3",
      "1950-1999"  = "#e7298a",
      "2000-2024"  = "#66a61e",
      "Unknown"    = "#666666"
    )
  ) +

  scale_y_continuous(
    limits = c(0, 100)
  ) +

  labs(
    title = "MIDS Element Completeness by Specimen Collection Period",
    subtitle = "Comparison of collection-year groups across MIDS elements",
    x = "",
    y = "Percentage of Records with Element = TRUE",
    fill = "Collection Period"
  ) +

  theme_bw() +

  theme(
    strip.text = element_text(
      face = "bold",
      size = 12
    ),

    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 7
    ),

    legend.position = "bottom"
  )

print(p)

#==================================================
# Save figure
#==================================================

ggsave(
  "analyses/Analysis10/MIDS_YearGroup_Comparison_All_Years.png",
  plot = p,
  width = 18,
  height = 14,
  dpi = 300
)

#==================================================
# Save TSV
#==================================================

write_tsv(
  all_years_results,
  "analyses/Analysis10/YearGroup_MIDS_Completeness_All_Years.tsv"
)