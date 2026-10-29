# classify datasets into Small / Medium / Large / Very Large, then show the proportion of records at MIDS 0, 1, 2 and 3 within each size category

library(arrow)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(scales)
library(viridis)

#==================================================
# Locate parquet files
#==================================================

files <- list.files(
  path = "outputs",
  pattern = "\\.parquet$",
  full.names = TRUE
)

#==================================================
# Extract years from filenames
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
# Calculate MIDS proportions by dataset size
#==================================================

mids_size_summary <- map_dfr(
  
  seq_along(files),
  
  function(i){
    
    year <- years[i]
    
    cat(
      "Processing",
      year,
      "\n"
    )
    
    #-----------------------------------------
    # Load required fields
    #-----------------------------------------
    
    df <- open_dataset(files[i]) |>
      select(
        datasetKey,
        MIDS_level
      ) |>
      collect()
    
    #-----------------------------------------
    # Calculate dataset sizes
    #-----------------------------------------
    
    dataset_sizes <- df |>
      
      count(
        datasetKey,
        name = "n_records"
      ) |>
      
      mutate(
        
        size_class = case_when(
          
          n_records < 10000 ~
            "Small",
          
          n_records < 100000 ~
            "Medium",
          
          n_records < 1000000 ~
            "Large",
          
          TRUE ~
            "Very Large"
          
        )
        
      )
    
    #-----------------------------------------
    # Attach size category to every record
    #-----------------------------------------
    
    df <- df |>
      
      left_join(
        dataset_sizes,
        by = "datasetKey"
      )
    
    #-----------------------------------------
    # Calculate proportions
    #-----------------------------------------
    
    df |>
      
      count(
        size_class,
        MIDS_level,
        name = "n_records"
      ) |>
      
      group_by(
        size_class
      ) |>
      
      mutate(
        pct_records =
          n_records / sum(n_records)
      ) |>
      
      ungroup() |>
      
      mutate(
        snapshot_year = year
      )
    
  }
  
)

#==================================================
# Save results
#==================================================

write.csv(
  mids_size_summary,
  "analyses/Analysis7/MIDS_by_Dataset_Size.csv",
  row.names = FALSE
)

#==================================================
# Ordering
#==================================================

mids_size_summary <- mids_size_summary |>
  
  mutate(
    
    size_class = factor(
      size_class,
      levels = c(
        "Small",
        "Medium",
        "Large",
        "Very Large"
      )
    ),
    
    MIDS_level = factor(
      MIDS_level,
      levels = c(
        -1, 0, 1, 2, 3
      )
    )
    
  )

#==================================================
# GRAPH 1
# One panel per size category
#==================================================

p1 <- ggplot(
  
  mids_size_summary,
  
  aes(
    factor(snapshot_year),
    pct_records,
    fill = MIDS_level
  )
  
) +
  
  geom_col(
    position = "fill"
  ) +
  
  facet_wrap(
    ~ size_class,
    ncol = 2
  ) +
  
  scale_y_continuous(
    labels = percent_format()
  ) +
  
  labs(
    title = "MIDS Distribution by Dataset Size",
    subtitle = "Proportion of records at each MIDS level",
    x = "Snapshot Year",
    y = "Proportion of Records",
    fill = "MIDS Level"
  ) +
  
  theme_bw()

print(p1)

#==================================================
# GRAPH 2
# One panel per year
#==================================================

p2 <- ggplot(
  
  mids_size_summary,
  
  aes(
    size_class,
    pct_records,
    fill = MIDS_level
  )
  
) +
  
  geom_col(
    position = "fill"
  ) +
  
  facet_wrap(
    ~ snapshot_year
  ) +
  
  scale_y_continuous(
    labels = percent_format()
  ) +
  
  labs(
    title = "MIDS Composition by Dataset Size",
    x = "Dataset Size",
    y = "Proportion of Records",
    fill = "MIDS Level"
  ) +
  
  theme_bw()

print(p2)



#==================================================
# GRAPH 4
# MIDS levels through time for each size class
#==================================================

p4 <- ggplot(
  
  mids_size_summary,
  
  aes(
    snapshot_year,
    pct_records,
    colour = MIDS_level,
    group = MIDS_level
  )
  
) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  geom_point(
    size = 2
  ) +
  
  facet_wrap(
    ~ size_class
  ) +
  
  scale_y_continuous(
    labels = percent_format()
  ) +
  
  labs(
    title = "Changes in MIDS Levels Through Time",
    subtitle = "Within each dataset size category",
    x = "Snapshot Year",
    y = "Proportion of Records",
    colour = "MIDS Level"
  ) +
  
  theme_bw()

print(p4)