# The script below:

# Loads all annual parquet files.
# Loads the continent mapping CSV.
# Maps countryCode → Continent_Name.
# Keeps the six populated continents:
# Africa
# Asia
# Europe
# North America
# Oceania
# South America
# Calculates MIDS element completeness for each continent and each year.
# Produces:
# A faceted plot with continent rows and year columns.
# A time-series plot showing completeness trends through time.

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
# Load continent lookup
#==================================================

continent_lookup <- read_csv(
  "https://gist.githubusercontent.com/stevewithington/20a69c0b6d2ff846ea5d35e5fc47f26c/raw/13716ceb2f22b5643ce5e7039643c86a0e0c6da6/country-and-continent-codes-list-csv.csv",
  show_col_types = FALSE
)

continent_lookup <- continent_lookup |>
  select(
    countryCode = Two_Letter_Country_Code,
    continent = Continent_Name
  )

target_continents <- c(
  "Africa",
  "Asia",
  "Europe",
  "North America",
  "Oceania",
  "South America"
)

#==================================================
# Process all years
#==================================================

all_years_results <- map_dfr(

  seq_along(files),

  function(i){

    year <- years[i]

    cat(
      "Processing ",
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

    #----------------------------------------------
    # Load only necessary columns
    #----------------------------------------------

df <- ds |>
  select(
    countryCode,
    all_of(bool_cols)
  ) |>
  filter(!is.na(countryCode)) |>
  collect()

    #----------------------------------------------
    # Join continent lookup
    #----------------------------------------------

    df <- df |>
      left_join(
        continent_lookup,
        by = "countryCode"
      ) |>
      filter(
        continent %in% target_continents
      )

    #----------------------------------------------
    # Summarise by continent
    #----------------------------------------------

    continent_results <- map_dfr(

      target_continents,

      function(current_continent){

        cat(
          "   ",
          current_continent,
          "\n"
        )

        subset_df <- df |>
          filter(
            continent == current_continent
          )

        total_rows <- nrow(subset_df)

        if(total_rows == 0){
          return(NULL)
        }

        tibble(
          continent = current_continent,
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

    continent_results |>

      mutate(

        true_percentage =
          100 * true_count / total_rows,

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
  "analyses/Analysis9/Continent_MIDS_Completeness_All_Years.csv",
  row.names = FALSE
)

#==================================================
# Save results
#==================================================

write.csv(
  all_years_results,
  "analyses/Analysis9/Continent_MIDS_Completeness_All_Years.csv",
  row.names = FALSE
)

#==================================================
# Order factors
#==================================================

all_years_results <- all_years_results |>

  mutate(

    continent = factor(
      continent,
      levels = c(
        "Africa",
        "Asia",
        "Europe",
        "North America",
        "Oceania",
        "South America"
      )
    )

  )

#==================================================
# Vertical comparison plot
# One panel per year
#==================================================

p <- ggplot(
  all_years_results,
  aes(
    x = column_name,
    y = true_percentage,
    fill = continent
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
      "Africa" = "#1b9e77",
      "Asia" = "#d95f02",
      "Europe" = "#7570b3",
      "North America" = "#e7298a",
      "Oceania" = "#66a61e",
      "South America" = "#e6ab02"
    )
  ) +

  scale_y_continuous(
    limits = c(0, 100)
  ) +

  labs(
    title = "MIDS Element Completeness by Continent Through Time",
    subtitle = "Comparison of continents across MIDS elements",
    x = "",
    y = "Percentage of Records with Element = TRUE",
    fill = "Continent"
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
  "analyses/Analysis9/MIDS_Continent_Comparison_All_Years.png",
  plot = p,
  width = 18,
  height = 14,
  dpi = 300
)

#==================================================
# Time-series plot
#==================================================

p_trends <- ggplot(
  all_years_results,
  aes(
    snapshot_year,
    true_percentage,
    colour = continent
  )
) +

  geom_line(
    linewidth = 1
  ) +

  geom_point(
    size = 1.5
  ) +

  facet_wrap(
    ~ column_name,
    scales = "free_y"
  ) +

  scale_x_continuous(
    breaks = years
  ) +

  scale_colour_manual(
    values = c(
      "Africa" = "#1b9e77",
      "Asia" = "#d95f02",
      "Europe" = "#7570b3",
      "North America" = "#e7298a",
      "Oceania" = "#66a61e",
      "South America" = "#e6ab02"
    )
  ) +

  labs(
    title = "MIDS Element Completeness Through Time by Continent",
    x = "Year",
    y = "% Records with Element = TRUE",
    colour = "Continent"
  ) +

  theme_bw()

print(p_trends)

#==================================================
# Save plot-ready data
#==================================================

write_tsv(
  all_years_results,
  "analyses/Analysis9/Continent_MIDS_Completeness_All_Years.tsv"
)