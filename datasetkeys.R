library(arrow)
library(tidyverse)

results_files = list.files("outputs",
                           pattern="*.parquet$",
                           full.names = T) %>%
  sort()

dkey_resu = list()

## overloads the CPU, but that should be fine (not the memory)
## commented to not accidentally run this again
# for (i in 1:length(results_files)) {
#   pq_conn = open_dataset(results_files[i])
#   dkey_resu[[i]] = pq_conn %>% 
#     count(datasetKey) %>% 
#     collect()
# }

library(jsonlite)

export_dkey = toJSON(dkey_resu,pretty = T)
write(export_dkey,"outputs/datasetkeys through the years.json")
#t=fromJSON("outputs/datasetkeys through the years.json")
