# Below is a complete version of the all-years Animalia/Plantae/Fungi comparison script using the improved visualisation:

# One panel per year.
# Horizontal bars.
# Three taxa side-by-side for each MIDS element.
# Consistent colours.
# Much easier comparison between taxa than separate kingdom panels.

library(arrow)
library(bit64)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readr)
library(stringr)
library(purrr)
library(rgbif)

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

ds_lookup <- open_dataset(
  files[length(files)]
)

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

write.csv(
  all_years_results,
  "analyses/Analysis8/Kingdom_MIDS_Completeness_All_Years.csv",
  row.names = FALSE
)

#==================================================
# Order factors
#==================================================

all_years_results <- all_years_results |>

  mutate(

    kingdom = factor(
      kingdom,
      levels = c(
        "Animalia",
        "Plantae",
        "Fungi"
      )
    ),

    column_name = factor(
      column_name,
      levels = rev(
        unique(column_name)
      )
    )

  )

#==================================================
# Horizontal comparison plot
#==================================================

p <- ggplot(
  all_years_results,
  aes(
    x = true_percentage,
    y = column_name,
    fill = kingdom
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
      "Animalia" = "#1b9e77",
      "Plantae"  = "#d95f02",
      "Fungi"    = "#7570b3"
    )
  ) +

  scale_x_continuous(
    limits = c(0, 100)
  ) +

  labs(
    title = "MIDS Element Completeness by Taxonomic Kingdom Through Time",
    subtitle = "Comparison of Animalia, Plantae and Fungi",
    x = "Percentage of Records with Element = TRUE",
    y = "",
    fill = "Kingdom"
  ) +

  theme_bw() +

  theme(
    strip.text = element_text(
      face = "bold",
      size = 12
    ),

    axis.text.y = element_text(
      size = 7
    ),

    legend.position = "bottom"
  )

print(p)

#==================================================
# Save figure
#==================================================

ggsave(
  "analyses/Analysis8/MIDS_Kingdom_Comparison_All_Years.png",
  plot = p,
  width = 18,
  height = 14,
  dpi = 300
)