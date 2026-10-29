# analysis2.R which was moved. This copied as a backup.

# This analysis focuses only on new records entering GBIF between consecutive snapshots, and then asks:
  
#   Of those new records, how many have each MIDS element = TRUE?
#     and
#   What percentage of new records have each MIDS element = TRUE?

library(arrow)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

#==================================================
# Locate parquet files
#==================================================

files <- list.files(
  path = "outputs",
  pattern = "\\.parquet$",
  full.names = TRUE
)

#==================================================
# Extract years
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
# Calculate TRUE counts for NEW records only
#==================================================

new_record_mids <- map_dfr(
  
  seq_len(length(files)-1),
  
  function(i){
    
    year1 <- years[i]
    year2 <- years[i + 1]
    
    cat(
      "Processing ",
      year1,
      " -> ",
      year2,
      "\n"
    )
    
    old_ids <- open_dataset(files[i]) |>
      select(gbifID) |>
      collect()
    
    current <- open_dataset(files[i + 1]) |>
      select(
        gbifID,
        all_of(mids_elements)
      ) |>
      collect()
    
    # New records only
    
    new_records <- anti_join(
      current,
      old_ids,
      by = "gbifID"
    )
    
    total_new <- nrow(new_records)
    
    map_dfr(
      
      mids_elements,
      
      function(element){
        
        true_count <- sum(
          new_records[[element]],
          na.rm = TRUE
        )
        
        tibble(
          interval =
            paste0(year1, "-", year2),
          
          year = year2,
          
          element =
            gsub(
              "mids:",
              "",
              element
            ),
          
          true_count =
            true_count,
          
          total_new =
            total_new,
          
          true_pct =
            true_count / total_new
          
        )
        
      }
      
    )
    
  }
  
)

# Save results

write.csv(
  new_record_mids,
  "analyses/Analysis2/New_Record_MIDS_Element_Completeness.csv",
  row.names = FALSE
)


# Graph 1: Total number of NEW records with TRUE values

# One panel per MIDS element.

ggplot(
  new_record_mids,
  aes(
    year,
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
    title = "New records reaching each MIDS Element",
    x = "Year",
    y = "Number of New Records"
  ) +
  theme_bw()

# Graph 2: Percentage of NEW records with TRUE values

# This is often the more informative figure.

ggplot(
  new_record_mids,
  aes(
    year,
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
    limits = c(0,1)
  ) +
  labs(
    title = "Completeness of new records by MIDS Element",
    x = "Year",
    y = "Percentage of New Records"
  ) +
  theme_bw()

