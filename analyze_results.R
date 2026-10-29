library(arrow)
library(bit64)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(magrittr)
library(readr)
library(ggbrace)

####Analysis
file_identifier = "mids_specimulti_2026-01-01_0070190-2511200835450852026-09-12 07.05PM"

filename = paste0("outputs/",
                  file_identifier,
                  ".parquet")
the_year = substr(file_identifier,17,20)
ds_test <- open_dataset(filename)

# Calculate frequencies instantly
small_int_frequencies <- ds_test |>
  count(MIDS_level) |>
  collect() |>
  mutate(percent = 100*n/sum(n))

# # Calculate frequencies instantly for specific subsets
# small_int_frequencies_be <- ds_test |>
#   filter(countryCode=="BE") |>
#   count(MIDS_level) |>
#   collect() |>
#   mutate(percent = 100*n/sum(n))
# 
# tracheophyta <- ds_test |>
#   filter(phylum == "Tracheophyta") |>
#   count(MIDS_level) |>
#   collect() |>
#   mutate(percent = 100*n/sum(n))

# 2. Compute the TRUE counts for all 18 columns in a single C++ pass
bool_cols <- names(ds_test)[sapply(ds_test$schema$fields, 
                                   function(f) f$type$ToString() == "bool")]
bool_summary_raw <- ds_test |>
  summarise(
    total_rows = n(),
    across(all_of(bool_cols), ~sum(as.integer(.x), na.rm = TRUE))
  ) |>
  collect()

# 3. Clean up the output into a nice, readable frequency table
final_boolean_table <- bool_summary_raw |>
  pivot_longer(
    cols = -total_rows, 
    names_to = "column_name", 
    values_to = "true_count"
  ) |>
  mutate(
    false_count = total_rows - true_count,
    true_percentage = (true_count / total_rows) * 100
  )

print(final_boolean_table)


### graph 

#shorten mids element labels
final_boolean_table %<>%
  mutate(column_name = gsub("mids:MIDS","",column_name,fixed=T))

# stack the table to show both the 0 and 1 counts
true_tbl = final_boolean_table %>%
  select(-false_count) %>%
  mutate(achieved = "1")

grap_tbl = final_boolean_table %>%
  select(-true_count) %>%
  mutate(achieved = "0") %>%
  rename(true_count = false_count) %>%
  rbind(true_tbl)

# function for wrapping barchart labels
wrap_hard_hyphen <- function(x, width = 10) {
  vapply(x, function(s) {
    # Split at hard width, insert hyphens
    parts <- substring(s, seq(1, nchar(s), by = width), seq(width, nchar(s) + width - 1, by = width))
    paste0(paste0(parts, ifelse(nchar(parts) == width, "-", "")), collapse = "\n")
  }, character(1))
}

# cutof coordinates to draw dashlines between MIDS levels
inf_elements = final_boolean_table %>%
  pull(column_name)

cutof1 = grep("1",inf_elements)[1] - 0.5
cutof2 = grep("2",inf_elements)[1] - 0.5
cutof3 = grep("3",inf_elements)[1] - 0.5

grap_tbl %<>%
  mutate(level = substr(column_name,1,1),
         element = substr(column_name,2,nchar(column_name)))

# # Plot the graph
# ggplot(grap_tbl, aes(x = element, y = true_count, fill = achieved)) +
#   theme_minimal(base_size=16) +
#   labs(x = "", 
#        y = "% of Specimens achieving the MIDS element", 
#        fill = "MIDS element achieved",
#        title=the_year) +
#   geom_bar(stat = "identity", position = "fill") +
#   facet_grid(
#     . ~ level,
#     scales = "free_x",
#     space = "free_x",
#     switch = "x"
#   ) +
#   #scale_x_discrete(labels = function(x) wrap_hard_hyphen(x)) +
#   scale_y_continuous(labels = percent_format()) +
#   theme(axis.text=element_text(size=16)) +
#   scale_fill_discrete(labels = c("No",
#                                  "Yes")) +
#   geom_hline(yintercept=0.50,linetype="dotted") +
#   geom_hline(yintercept=0.75,linetype="dotted") +
#   geom_hline(yintercept=0.25,linetype="dotted") +
#   theme(
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     strip.text.x = element_text(face = "bold"),
#     
#     panel.spacing.x = unit(0, "pt"),
#     panel.border = element_rect(
#       colour = "grey70",
#       fill = NA,
#       linewidth = 0.5
#     ),
#     axis.text.x = element_text(
#       size = 16,
#       angle = 45,
#       hjust = 1,
#       vjust = 1
#     ),
#     
#     plot.title = element_text(hjust = 0.5),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor = element_blank(),
#     
#     legend.position = "right"
#   )

# Count the distinct elements in each level
brace_counts <- grap_tbl |>
  distinct(level, element) |>
  count(level, name = "n")

# Coordinates for the ends of each brace
brace_df <- data.frame(
  level = rep(brace_counts$level, each = 2),
  x = as.vector(rbind(
    rep(0.55, nrow(brace_counts)),
    brace_counts$n + 0.45
  )),
  y = 1
)

ggplot(grap_tbl, aes(x = element, y = true_count, fill = achieved)) +
  theme_minimal(base_size = 16) +
  labs(
    x = "",
    y = "% of Specimens achieving the MIDS element",
    fill = "MIDS element achieved",
    title = the_year
  ) +
  geom_bar(stat = "identity", position = "fill") +
  
  # Curly brace above each group of bars
  ggbrace::stat_brace(
    data = brace_df,
    aes(x = x, y = y, group = 1),
    inherit.aes = FALSE,
    rotate = 0,
    outerstart = 1.04,
    width = 0.045,
    linewidth = 0.8
  ) +
  
  facet_grid(
    . ~ level,
    scales = "free_x",
    space = "free_x"
  ) +
  
  # Extra vertical space for the braces
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    labels = percent_format(),
    expand = expansion(mult = 0)
  ) +
  coord_cartesian(ylim = c(0, 1.14)) +
  
  scale_fill_discrete(labels = c("No", "Yes")) +
  
  geom_hline(yintercept = c(0.25, 0.50, 0.75),
             linetype = "dotted") +
  
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.x = element_text(
      face = "bold",
      size = 18
    ),
    
    panel.spacing.x = unit(0, "pt"),
    panel.border = element_rect(
      colour = "grey70",
      fill = NA,
      linewidth = 0.5
    ),
    
    axis.text = element_text(size = 16),
    axis.text.x = element_text(
      size = 16,
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# concatenate mids level values to the element binary map
level_values = small_int_frequencies %>%
  rename(column_name = MIDS_level,true_count = n, true_percentage = percent) %>%
  mutate(total_rows = final_boolean_table$total_rows[1],
         false_count = NA) %>%
  select(all_of(colnames(final_boolean_table)))

# save the summary data as a simple csv
write_tsv(rbind(final_boolean_table,level_values),paste0(filename,"__simple.csv"),na="")
