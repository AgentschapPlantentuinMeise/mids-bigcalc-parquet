library(arrow)
library(bit64)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(magrittr)
library(readr)
library(duckdb)

results_files = list.files("outputs",
                           pattern="*.parquet$",
                           full.names = T) %>%
  sort()
all_new = tibble(MIDS_level = numeric(0),
                 n=numeric(0),
                 percent=numeric(0),
                 year=character(0))
for (i in 2:length(results_files)) {
  con <- dbConnect(duckdb::duckdb())
  dbExecute(con, "SET max_memory = '16GB'")
  
  small_int_frequencies <- open_dataset(results_files[i]) %>%
    to_duckdb(con = con) %>%
    anti_join(open_dataset(results_files[i-1]) %>% 
                select(gbifID) %>% 
                to_duckdb(con = con), 
              by = "gbifID") %>%
    count(MIDS_level) %>%
    collect() %>%
    mutate(percent = 100*n/sum(n))
  
  small_int_frequencies$year = substr(results_files[i],25,28)
  all_new = rbind(all_new,
                  small_int_frequencies)
  dbDisconnect(con, shutdown = TRUE)
  print(i)
}

write_tsv(all_new,"outputs/mids_scores_new_records.txt")

all_new$level=as.character(all_new$MIDS_level)

ggplot(
  all_new,
  aes(x = level, y = percent, fill = level)
) +
  geom_col(width = 0.8) +
  
  # Each year becomes a group of bars
  facet_grid(
    . ~ year,
    scales = "free_x",
    space = "free_x",
    switch = "x"
  ) +
  
  # true_percentage is already 0-100
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    x = NULL,
    y = "Percentage of new GBIF Specimen records",
    fill = "MIDS Level"
  ) +
  
  theme_minimal(base_size = 24) +
  theme(
    # Put year below the level labels
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.x = element_text(face = "bold"),
    
    # Make boundaries between years visible
    panel.spacing.x = unit(0, "pt"),
    panel.border = element_rect(
      colour = "grey70",
      fill = NA,
      linewidth = 0.5
    ),
    
    # Cleaner appearance
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.position = "right"
  )

ggplot(
  all_new,
  aes(x = level, y = n/100000, fill = level)
) +
  geom_col(width = 0.8) +
  
  # Each year becomes a group of bars
  facet_grid(
    . ~ year,
    scales = "free_x",
    space = "free_x",
    switch = "x"
  ) +
  
  labs(
    x = NULL,
    y = "New GBIF Specimen records (100.000s)",
    fill = "MIDS Level"
  ) +
  
  theme_minimal(base_size = 24) +
  theme(
    # Put year below the level labels
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.x = element_text(face = "bold"),
    
    # Make boundaries between years visible
    panel.spacing.x = unit(0, "pt"),
    panel.border = element_rect(
      colour = "grey70",
      fill = NA,
      linewidth = 0.5
    ),
    
    # Cleaner appearance
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.position = "right"
  )