# Below is a complete version for a single year (e.g. 2022) that compares Animalia, Plantae, and Fungi, calculates MIDS element completeness for each kingdom, and produces a faceted plot with one panel per kingdom.

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

#==================================================
# Locate parquet files
#==================================================

files <- list.files(
  path = "outputs",
  pattern = "\\.parquet$",
  full.names = TRUE
)

#==================================================
# Select year
#==================================================

analysis_year <- 2022

filename <- files[
  str_detect(
    basename(files),
    paste0("^", analysis_year)
  )
]

cat(
  "Using file:\n",
  basename(filename),
  "\n"
)

#==================================================
# Open dataset
#==================================================

ds_test <- open_dataset(filename)

#==================================================
# Build phylum lookup
#==================================================

phyla <- ds_test |>
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

#==================================================
# Target kingdoms
#==================================================

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
# Boolean columns
#==================================================

bool_cols <- names(ds_test)[

  sapply(
    ds_test$schema$fields,
    function(f)
      f$type$ToString() == "bool"
  )

]

#==================================================
# Calculate completeness by kingdom
#==================================================

kingdom_summaries <- lapply(

  target_kingdoms,

  function(current_kingdom){

    cat(
      "Processing kingdom:",
      current_kingdom,
      "\n"
    )

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
        kingdom = current_kingdom
      )

  }

)

bool_summary_raw <- bind_rows(
  kingdom_summaries
)

#==================================================
# Convert to long format
#==================================================

final_boolean_table <- bool_summary_raw |>

  pivot_longer(
    cols = all_of(bool_cols),
    names_to = "column_name",
    values_to = "true_count"
  ) |>

  mutate(

    false_count =
      total_rows - true_count,

    true_percentage =
      100 * true_count / total_rows

  )

#==================================================
# Shorten labels
#==================================================

final_boolean_table <- final_boolean_table |>

  mutate(
    column_name =
      gsub(
        "mids:MIDS",
        "",
        column_name,
        fixed = TRUE
      )
  )

#==================================================
# Label wrapping function
#==================================================

wrap_hard_hyphen <- function(x, width = 10) {

  vapply(
    x,
    function(s) {

      parts <- substring(
        s,
        seq(1, nchar(s), by = width),
        seq(width, nchar(s) + width - 1, by = width)
      )

      paste0(
        paste0(
          parts,
          ifelse(
            nchar(parts) == width,
            "-",
            ""
          )
        ),
        collapse = "\n"
      )

    },
    character(1)
  )

}

#==================================================
# Create faceted plot
#==================================================

p <- ggplot(
  final_boolean_table,
  aes(
    x = column_name,
    y = true_percentage,
    fill = kingdom
  )
) +

  geom_col() +

  facet_wrap(
    ~ kingdom,
    ncol = 1
  ) +

  scale_x_discrete(
    labels = function(x)
      wrap_hard_hyphen(x)
  ) +

  scale_y_continuous(
    labels = function(x)
      paste0(x, "%")
  ) +

  labs(
    title = paste(
      "MIDS Element Completeness by Kingdom",
      analysis_year
    ),
    x = "",
    y = "Percentage of Records with Element = TRUE",
    fill = "Kingdom"
  ) +

  theme_bw() +

  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1
    ),
    strip.text = element_text(
      face = "bold",
      size = 12
    )
  )

print(p)

#==================================================
# Save graph
#==================================================

ggsave(
  paste0(
    "MIDS_Animalia_Plantae_Fungi_",
    analysis_year,
    ".png"
  ),
  p,
  width = 14,
  height = 10,
  dpi = 300
)

#==================================================
# Save summary table
#==================================================

write_tsv(
  final_boolean_table,
  paste0(
    "MIDS_Animalia_Plantae_Fungi_",
    analysis_year,
    "_summary.tsv"
  ),
  na = ""
)
