# The script below will:
  
# Read every yearly parquet file.
# Extract the year from the filename.
# Count the number of records where each MIDS element is TRUE.
# Create a dataset for plotting.



library(arrow)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

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
# MIDS elements
#==================================================

mids_elements <- c(
  "mids:MIDS0PhysicalSpecimenID",
  "mids:MIDS0Organization",
  
  "mids:MIDS1Name",
  "mids:MIDS1ObjectType",
  "mids:MIDS1License",
  "mids:MIDS1Modified",
  
  "mids:MIDS2QualitativeLocation",
  "mids:MIDS2QuantitativeLocation",
  "mids:MIDS2CollectingAgent",
  "mids:MIDS2DateCollected",
  "mids:MIDS2CollectingNumber",
  "mids:MIDS2Media",
  
  "mids:MIDS3GeographicalLocalityID",
  "mids:MIDS3OrganizationID",
  "mids:MIDS3CollectingAgentID",
  "mids:MIDS3IdentifiedAsID",
  "mids:MIDS3IdentifiedByID",
  "mids:MIDS3MediaID"
)

#==================================================
# Count TRUE values for each element in each year
#==================================================

mids_counts <- map_dfr(
  
  seq_along(files),
  
  function(i){
    
    year <- years[i]
    
    cat("Processing", year, "\n")
    
    df <- open_dataset(files[i]) |>
      select(all_of(mids_elements)) |>
      collect()
    
    map_dfr(
      
      mids_elements,
      
      function(element){
        
        true_count <- sum(
          df[[element]],
          na.rm = TRUE
        )
        
        tibble(
          snapshot_year = year,
          element = gsub("mids:", "", element),
          true_count = true_count
        )
        
      }
      
    )
    
  }
  
)

#==================================================
# Save results
#==================================================

write.csv(
  mids_counts,
  "analyses/Analysis1/MIDS_Element_True_Counts.csv",
  row.names = FALSE
)

#==================================================
# Add MIDS groups
#==================================================

mids_counts <- mids_counts |>
  mutate(
    mids_group = case_when(
      grepl("^MIDS-1", element) ~ "MIDS -1",
      grepl("^MIDS0", element) ~ "MIDS 0",
      grepl("^MIDS1", element) ~ "MIDS 1",
      grepl("^MIDS2", element) ~ "MIDS 2",
      grepl("^MIDS3", element) ~ "MIDS 3"
    )
  )

#mids_percentages <- mids_percentages |>
#mutate(
#    mids_group = case_when(
#      grepl("^MIDS0", element) ~ "MIDS 0",
#      grepl("^MIDS1", element) ~ "MIDS 1",
#      grepl("^MIDS2", element) ~ "MIDS 2",
#      grepl("^MIDS3", element) ~ "MIDS 3"
#    )
#  )


#==================================================
# ANALYSIS 1a
# Total number of records with TRUE values
# This will produce four separate panels:
# MIDS 0
# MIDS 1
# MIDS 2
# MIDS 3
# with each element represented by a separate colour.
#==================================================

library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)

# Create one graph per MIDS level

for(group in unique(mids_counts$mids_group)) {
  
  plot_data <- mids_counts |>
    filter(
      mids_group == group
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = snapshot_year,
      y = true_count,
      colour = element,
      group = element
    )
  ) +
    
    geom_line(
      linewidth = 1.4
    ) +
    
    geom_point(
      size = 3
    ) +
    
    #    scale_colour_brewer(
    #      palette = "Set1"
    #    ) +
    scale_colour_viridis_d(
      option = "turbo"
    ) +
    
    scale_x_continuous(
      breaks = sort(
        unique(plot_data$snapshot_year)
      ),
      labels = sort(
        unique(plot_data$snapshot_year)
      )
    ) +
    
    scale_y_continuous(
      labels = comma
    ) +
    
    labs(
      title = paste(
        "Total number of records reaching",
        group
      ),
#      subtitle = "Total records reaching each MIDS element",
      x = "Year",
      y = "Number of Records",
      colour = "MIDS Element"
    ) +
    
    theme_bw() +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 14
      ),
      
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      
      legend.position = "bottom",
      
      legend.title = element_text(
        face = "bold"
      )
    )
  
  print(p)
  
}

#==================================================
# ANALYSIS 1b
# Percentage of records with TRUE values
# This will produce four separate panels:
# MIDS 0
# MIDS 1
# MIDS 2
# MIDS 3
# with each element represented by a separate colour.
#==================================================


library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)

# Calculate annual totals

total_records <- map_dfr(
  
  seq_along(files),
  
  function(i){
    
    year <- years[i]
    
    n_records <- open_dataset(files[i]) |>
      select(gbifID) |>
      collect() |>
      nrow()
    
    tibble(
      snapshot_year = year,
      total_records = n_records
    )
    
  }
  
)

# Create mids_percentages

mids_percentages <- mids_counts |>
  
  left_join(
    total_records,
    by = "snapshot_year"
  ) |>
  
  mutate(
    true_pct = true_count / total_records
  )

# Add MIDS groups

mids_percentages <- mids_percentages |>
  mutate(
    mids_group = case_when(
      grepl("^MIDS-1", element) ~ "MIDS -1",
      grepl("^MIDS0", element) ~ "MIDS 0",
      grepl("^MIDS1", element) ~ "MIDS 1",
      grepl("^MIDS2", element) ~ "MIDS 2",
      grepl("^MIDS3", element) ~ "MIDS 3"
    )
  )

# Create one graph per MIDS level

for(group in unique(mids_percentages$mids_group)) {
  
  plot_data <- mids_percentages |>
    filter(
      mids_group == group
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = snapshot_year,
      y = true_pct,
      colour = element,
      group = element
    )
  ) +
    
    geom_line(
      linewidth = 1.4
    ) +
    
    geom_point(
      size = 3
    ) +
    
#    scale_colour_brewer(
#      palette = "Set1"
#    ) +
    scale_colour_viridis_d(
      option = "turbo"
    ) +
    
    scale_x_continuous(
      breaks = sort(
        unique(plot_data$snapshot_year)
      ),
      labels = sort(
        unique(plot_data$snapshot_year)
      )
    ) +
    
    scale_y_continuous(
      labels = percent_format(),
      limits = c(0, 1)
    ) +
    
    labs(
      title = paste(
        "Completeness of",
        group,
        "elements through time"
      ),
      subtitle = "Percentage of records reaching each MIDS element",
      x = "Year",
      y = "Percentage of Records",
      colour = "MIDS Element"
    ) +
    
    theme_bw() +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 14
      ),
      
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      
      legend.position = "bottom",
      
      legend.title = element_text(
        face = "bold"
      )
    )
  
  print(p)
  
}

# # Separate graph for each MIDS level
# 
# for(group in unique(mids_percentages$mids_group)) {
#   
#   p <- mids_percentages |>
#     
#     filter(
#       mids_group == group
#     ) |>
#     
#     ggplot(
#       aes(
#         snapshot_year,
#         true_pct,
#         colour = element,
#         group = element
#       )
#     ) +
#     
#     geom_line(
#       linewidth = 1.2
#     ) +
#     
#     geom_point(
#       size = 3
#     ) +
#     
#     scale_x_continuous(
#       breaks = sort(
#         unique(
#           mids_percentages$snapshot_year
#         )
#       )
#     ) +
#     
#     scale_y_continuous(
#       labels = scales::percent_format(),
#       limits = c(0, 1)
#     ) +
#     
#     labs(
#       title = group,
#       x = "Year",
#       y = "Percentage of Records",
#       colour = "Element"
#     ) +
#     
#     theme_bw()
#   
#   print(p)
#   
# }

#==================================================
# Ungrouped by MIDS Level
#==================================================


# Faceted graph (one panel per MIDS element)


ggplot(
  mids_counts,
  aes(
    snapshot_year,
    true_count
  )
) +
  geom_line(
    colour = "steelblue",
    linewidth = 1
  ) +
  geom_point(
    colour = "steelblue",
    size = 2
  ) +
  facet_wrap(
    ~ element,
    scales = "free_y"
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Total number of records reaching each MIDS Element through time",
    x = "Year",
    y = "Number of Records"
  ) +
  theme_bw()

# Separate graph for each MIDS element

# for(elem in unique(mids_counts$element)) {
#   
#   p <- mids_counts |>
#     
#     filter(element == elem) |>
#     
#     ggplot(
#       aes(
#         snapshot_year,
#         true_count
#       )
#     ) +
#     
#     geom_line(
#       colour = "steelblue",
#       linewidth = 1.2
#     ) +
#     
#     geom_point(
#       colour = "steelblue",
#       size = 3
#     ) +
#     
#     scale_y_continuous(
#       labels = comma
#     ) +
#     
#     labs(
#       title = elem,
#       x = "Year",
#       y = "Number of Records with TRUE"
#     ) +
#     
#     theme_bw()
#   
#   print(p)
#   
# }

# Calculate percentages

# This adds the total number of records in each yearly snapshot and calculates:
# percentage = TRUE records / total records

#==================================================
# Calculate total records in each year
#==================================================

total_records <- map_dfr(
  
  seq_along(files),
  
  function(i){
    
    year <- years[i]
    
    n_records <- open_dataset(files[i]) |>
      select(gbifID) |>
      collect() |>
      nrow()
    
    tibble(
      snapshot_year = year,
      total_records = n_records
    )
    
  }
  
)

#==================================================
# Join totals and calculate percentages
#==================================================

#==================================================
# Calculate total records in each year
#==================================================

total_records <- map_dfr(
  
  seq_along(files),
  
  function(i){
    
    year <- years[i]
    
    n_records <- open_dataset(files[i]) |>
      select(gbifID) |>
      collect() |>
      nrow()
    
    tibble(
      snapshot_year = year,
      total_records = n_records
    )
    
  }
  
)

#==================================================
# Join totals and calculate percentages
#==================================================

mids_percentages <- mids_counts |>
  
  left_join(
    total_records,
    by = "snapshot_year"
  ) |>
  
  mutate(
    true_pct =
      true_count / total_records
  )

# Faceted graph of percentages

ggplot(
  mids_percentages,
  aes(
    snapshot_year,
    true_pct
  )
) +
  geom_line(
    colour = "darkgreen",
    linewidth = 1
  ) +
  geom_point(
    colour = "darkgreen",
    size = 2
  ) +
  facet_wrap(
    ~ element
  ) +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 1)
  ) +
  labs(
    title = "Completeness of MIDS Elements Through Time",
    x = "Year",
    y = "Percentage of Records"
  ) +
  theme_bw()

