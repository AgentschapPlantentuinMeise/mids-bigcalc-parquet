library(arrow)
library(bit64)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(magrittr)
library(readr)

####Analysis
filename = paste0("outputs/",
                  "mids_specimulti_2026-01-01_0070190-2511200835450852026-09-08 05.20PM",
                  ".parquet")
ds_test <- open_dataset(filename)

# Calculate frequencies instantly
small_int_frequencies <- ds_test |>
  count(MIDS_level) |>
  collect() |>
  mutate(percent = 100*n/sum(n))

# phyla <- ds_test |>
#   count(phylum) |>
#   collect() 

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
bool_cols <- names(ds_test)[sapply(ds_test$schema$fields, function(f) f$type$ToString() == "bool")]
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

# Plot the graph
ggplot(grap_tbl, aes(x = column_name, y = true_count, fill = achieved)) +
  labs(x = "", y = "% of Specimens achieving the MIDS element", fill = "MIDS element achieved") +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_discrete(labels = function(x) wrap_hard_hyphen(x)) +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text=element_text(size=12)) +
  scale_fill_discrete(labels = c("No",
                                 "Yes")) +
  geom_hline(yintercept=0.50,linetype="dotted") +
  geom_hline(yintercept=0.75,linetype="dotted") +
  geom_hline(yintercept=0.25,linetype="dotted") +
  geom_vline(xintercept=cutof1,linetype="dashed",linewidth=1) +
  geom_vline(xintercept=cutof2,linetype="dashed",linewidth=1) +
  geom_vline(xintercept=cutof3,linetype="dashed",linewidth=1)

# concatenate mids level values to the element binary map
level_values = small_int_frequencies %>%
  rename(column_name = MIDS_level,true_count = n, true_percentage = percent) %>%
  mutate(total_rows = final_boolean_table$total_rows[1],
         false_count = NA) %>%
  select(all_of(colnames(final_boolean_table)))

# save the summary data as a simple csv
write_tsv(rbind(final_boolean_table,level_values),paste0(filename,"__simple.csv"),na="")
