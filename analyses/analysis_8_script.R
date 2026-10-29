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
# Extract years and sort
#==================================================

# years <- as.integer(
#   str_extract(
#     basename(files),
#     "^\\d{4}"
#   )
# )

# ord <- order(years)

# files <- files[ord]
# years <- years[ord]


#==================================================
# Select a single year
#==================================================

analysis_year <- 2026

filename <- files[
  years == analysis_year
]

cat(
  "Using file:\n",
  basename(filename),
  "\n"
)

ds_test <- open_dataset(filename)

#==================================================
# Function for wrapping labels
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
# Process each year
#==================================================

for(i in seq_along(files)) {

  year <- years[i]
  filename <- files[i]

  cat(
    "\n=====================================\n",
    "Processing ",
    year,
    "\n=====================================\n"
  )

  ds_test <- open_dataset(filename)

  #------------------------------------------------
  # Build phylum lookup
  #------------------------------------------------

  phyla <- ds_test |>
    count(phylum) |>
    collect()

  phyla$kingdom <- NA

  for(j in seq_len(nrow(phyla))) {

    if(is.na(phyla$phylum[j])) next

    bb_lookup <- tryCatch(
      name_backbone(
        phyla$phylum[j]
      ),
      error = function(e) NULL
    )

    if(
      !is.null(bb_lookup) &&
      !is.null(bb_lookup$kingdom)
    ) {

      phyla$kingdom[j] <-
        bb_lookup$kingdom[1]

    }

  }

  animals <- phyla |>
    filter(
      kingdom == "Animalia"
    ) |>
    pull(phylum)

  #------------------------------------------------
  # Identify boolean columns
  #------------------------------------------------

  bool_cols <- names(ds_test)[

    sapply(
      ds_test$schema$fields,
      function(f)
        f$type$ToString() == "bool"
    )

  ]

  #------------------------------------------------
  # Count TRUE values
  #------------------------------------------------

  bool_summary_raw <- ds_test |>

    filter(
      phylum %in% animals
    ) |>

    summarise(

      total_rows = n(),

      across(
        all_of(bool_cols),
        ~ sum(as.integer(.x),
              na.rm = TRUE)
      )

    ) |>

    collect()

  #------------------------------------------------
  # Convert to long table
  #------------------------------------------------

  final_boolean_table <- bool_summary_raw |>

    pivot_longer(
      cols = -total_rows,
      names_to = "column_name",
      values_to = "true_count"
    ) |>

    mutate(

      false_count =
        total_rows - true_count,

      true_percentage =
        (true_count / total_rows) * 100

    )

  #------------------------------------------------
  # MIDS distribution
  #------------------------------------------------

  small_int_frequencies <- ds_test |>

    filter(
      phylum %in% animals
    ) |>

    count(MIDS_level) |>

    collect() |>

    mutate(
      percent =
        100 * n / sum(n)
    )

  #------------------------------------------------
  # Clean labels
  #------------------------------------------------

  final_boolean_table %<>%
    mutate(
      column_name =
        gsub(
          "mids:MIDS",
          "",
          column_name,
          fixed = TRUE
        )
    )

  #------------------------------------------------
  # Build plotting table
  #------------------------------------------------

  true_tbl <- final_boolean_table |>

    select(
      -false_count
    ) |>

    mutate(
      achieved = "1"
    )

  grap_tbl <- final_boolean_table |>

    select(
      -true_count
    ) |>

    mutate(
      achieved = "0"
    ) |>

    rename(
      true_count = false_count
    ) |>

    bind_rows(
      true_tbl
    )

  #------------------------------------------------
  # MIDS separators
  #------------------------------------------------

  inf_elements <- final_boolean_table$column_name

  cutof1 <- grep(
    "^1",
    inf_elements
  )[1] - 0.5

  cutof2 <- grep(
    "^2",
    inf_elements
  )[1] - 0.5

  cutof3 <- grep(
    "^3",
    inf_elements
  )[1] - 0.5

  #------------------------------------------------
  # Create graph
  #------------------------------------------------

  p <- ggplot(
    grap_tbl,
    aes(
      x = column_name,
      y = true_count,
      fill = achieved
    )
  ) +

    geom_bar(
      stat = "identity",
      position = "fill"
    ) +

    scale_x_discrete(
      labels = function(x)
        wrap_hard_hyphen(x)
    ) +

    scale_y_continuous(
      labels = percent_format()
    ) +

    scale_fill_discrete(
      labels = c(
        "No",
        "Yes"
      )
    ) +

    geom_hline(
      yintercept = 0.25,
      linetype = "dotted"
    ) +

    geom_hline(
      yintercept = 0.50,
      linetype = "dotted"
    ) +

    geom_hline(
      yintercept = 0.75,
      linetype = "dotted"
    ) +

    geom_vline(
      xintercept = cutof1,
      linetype = "dashed",
      linewidth = 1
    ) +

    geom_vline(
      xintercept = cutof2,
      linetype = "dashed",
      linewidth = 1
    ) +

    geom_vline(
      xintercept = cutof3,
      linetype = "dashed",
      linewidth = 1
    ) +

    labs(
      title = paste(
        "Animalia MIDS Completeness",
        year
      ),
      x = "",
      y = "% of Animal Specimens Achieving the MIDS Element",
      fill = "MIDS Element Achieved"
    ) +

    theme_bw() +

    theme(
      axis.text = element_text(
        size = 12
      )
    )

  print(p)

  #------------------------------------------------
  # Save figure
  #------------------------------------------------

  ggsave(
    paste0(
      "Animalia_MIDS_",
      year,
      ".png"
    ),
    p,
    width = 14,
    height = 8,
    dpi = 300
  )

  #------------------------------------------------
  # Add MIDS level frequencies
  #------------------------------------------------

  level_values <- small_int_frequencies |>

    rename(
      column_name = MIDS_level,
      true_count = n,
      true_percentage = percent
    ) |>

    mutate(
      total_rows =
        final_boolean_table$total_rows[1],
      false_count = NA
    ) |>

    select(
      all_of(
        colnames(
          final_boolean_table
        )
      )
    )

  #------------------------------------------------
  # Save summary table
  #------------------------------------------------

  write_tsv(
    bind_rows(
      final_boolean_table,
      level_values
    ),
    paste0(
      "Animalia_MIDS_",
      year,
      "_summary.tsv"
    ),
    na = ""
  )

}