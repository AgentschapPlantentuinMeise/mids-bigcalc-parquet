# Build the phylum → kingdom lookup once.
# Loop through every annual parquet file.
# Calculate completeness for Animalia, Plantae and Fungi.
# Add the year as a variable.
# Combine everything into one results table.
# Produce:
# A faceted plot with panels for year × kingdom.
# A second plot showing trends through time for each MIDS element.

library(arrow)
library(bit64)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(magrittr)
library(readr)
library(stringr)
library(rgbif)
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
# Build phylum lookup ONCE
#==================================================

cat("Building phylum lookup...\n")

ds_lookup <- open_dataset(files[length(files)])

phyla <- ds_lookup |>
  count(phylum) |>
  collect()

phyla$kingdom <- NA_character_

for(i in seq_len(nrow(phyla))) {

  if(is.na(phyla$phylum[i])) next

  bb_lookup <- tryCatch(
    name_backbone(
      phyla$phylum[i]
    ),
    error = function(e) NULL
  )

  if(
    !is.null(bb_lookup) &&
    !is.null(bb_lookup$kingdom)
  ) {

    phyla$kingdom[i] <- bb_lookup$kingdom[1]

  }

}

target_kingdoms <- c(
  "Animalia",
  "Plantae",
  "Fungi"
)

kingdom_lookup <- phyla |>
  filter(
    kingdom %in% target_kingdoms
  ) |>
  select(
    phylum,
    kingdom
  )

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

    ds_test <- open_dataset(
      files[i]
    )

    bool_cols <- names(ds_test)[

      sapply(
        ds_test$schema$fields,
        function(f)
          f$type$ToString() == "bool"
      )

    ]

    kingdom_summaries <- lapply(

      target_kingdoms,

      function(current_kingdom){

        current_phyla <- kingdom_lookup |>
          filter(
            kingdom == current_kingdom
          ) |>
          pull(phylum)

        ds_test |>

          filter(
            phylum %in% current_phyla
          ) |>

          summarise(

            total_rows = n(),

            across(
              all_of(bool_cols),
              ~ sum(
                as.integer(.x),
                na.rm = TRUE
              )
            )

          ) |>

          collect() |>

          mutate(
            kingdom = current_kingdom,
            snapshot_year = year
          )

      }

    )

    bind_rows(
      kingdom_summaries
    ) |>

      pivot_longer(
        cols = all_of(bool_cols),
        names_to = "column_name",
        values_to = "true_count"
      ) |>

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
# Save complete dataset
#==================================================

write_tsv(
  all_years_results,
  "Kingdom_MIDS_Completeness_All_Years.tsv"
)

#==================================================
# Faceted plot
#==================================================

ggplot(
  all_years_results,
  aes(
    column_name,
    true_percentage,
    fill = kingdom
  )
) +

  geom_col() +

  facet_grid(
    kingdom ~ snapshot_year
  ) +

  scale_y_continuous(
    labels = function(x)
      paste0(x, "%")
  ) +

  labs(
    title =
      "MIDS Element Completeness Through Time",
    x = "",
    y = "% Records with Element = TRUE"
  ) +

  theme_bw() +

  theme(
    axis.text.x =
      element_text(
        angle = 90,
        hjust = 1,
        size = 6
      )
  )

#==================================================
# Trends through time
#==================================================

ggplot(
  all_years_results,
  aes(
    snapshot_year,
    true_percentage,
    colour = kingdom
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

  labs(
    title =
      "MIDS Completeness Trends Through Time",
    x = "Year",
   