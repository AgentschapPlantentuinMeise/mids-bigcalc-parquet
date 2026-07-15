library(arrow)
library(bit64)

# remove NULL chars as they may cause errors in the C++ backend
options(arrow.skip_nul = TRUE)

#dim(ds)
#schema(ds)

# path to the MIDSCalculator source code, to re-use for SSSOM parsing
calcpath = "../../mathias/MIDSCalculator/src/"

# load the same packages
# this includes the rshiny stuff, so some overhead
source(paste0(calcpath,"packages.R"))
pkgLoad()

# config file to use the correct sssom mappings
config = read.ini("config.ini")

# load the parsing functions from the MIDSCalculator
source(paste0(calcpath,"parse_json_schema.R"))
source(paste0(calcpath,"parse_data_formats.R"))
source(paste0(calcpath,"MIDS-calc.R"))

# load the local sssom mapping into a json schema compatible with the
# midscalculator code
# localpath = T ensures the mapping is taken from this repo, not the calculator one
# this is needed as the relative paths in the MIDSCalculator code cause problems
schema = parse_sssom(config = config,localpath=T)

#
## Copied code from midscalculator below, to create the lists used during the
## calculation process
list_UoM <- list()
#Loop trough the values
n_values <- length(schema$unknownOrMissing)
for (l in 1:n_values) {
  value = schema$unknownOrMissing[[l]]$value[[1]]
  #only take into account values which do not count for mids
  if (schema$unknownOrMissing[[l]]$midsAchieved == FALSE) {
    #check if there is a property, otherwise it relates to all properties
    if ("property" %in% names(schema$unknownOrMissing[[l]])){
      prop <- schema$unknownOrMissing[[l]]$property[[1]]
    } else {
      prop <- "all"
    }
    #add to list
    if (prop %in% names(list_UoM)){
      list_UoM[[prop]] <- append(list_UoM[[prop]], value)
    } else {
      list_UoM[[prop]] <- value
    }
  }
}

midsschema <- schema[grep("mids", names(schema))]
list_criteria <- list()
list_props <- character()
#Loop trough sections
for (sect_index in seq_along(names(midsschema))){
  #Get the contents of a section
  section <- midsschema[[sect_index]] 
  #Loop through conditions
  for (cond_index in seq_along(names(section))){ 
    crits <- ""
    condition_name = names(section)[cond_index]
    # Loop trough subconditions, one of these should be true (|)
    for (subcond_index in seq_along(section[[condition_name]])){
      # get the contents (properties etc) of a single subcondition
      subcondition <- section[[condition_name]][[subcond_index]] 
      #if operator is NOT, inverse all the criteria
      if ("operator" %in% names(subcondition) && subcondition$operator == "NOT"){
        crits <- paste0(crits, "!")}
      #open brackets before the subcondition
      if (subcond_index == 1){crits <- paste0(crits, "(")}
      # Loop trough properties
      for (prop_index in seq_along(subcondition$property)){
        prop <- subcondition$property[[prop_index]]
        if (is.null(prop)){break} #if there is no property, exit this loop iterating over props, still need to check later what to do when there is no property
        #make a list of properties
        list_props <- append(list_props, prop)
        #add the property is not na
        crits <- paste0(crits, "!is.na(`", prop, "`)")
        #if there is a operator and it is not the last property, then add the matching operator to the string
        #currently does not work if there are subconditions without property! needs to be fixed 
        if (prop_index != length(subcondition$property) & "operator" %in% names(subcondition)){
          if (subcondition$operator == "OR"){crits <- paste0(crits, " | ")}
          if (subcondition$operator == "AND"){crits <- paste0(crits, " & ")}
        }
      }
      #Add | between subconditions
      if (subcond_index != length(section[[condition_name]])){crits <- paste0(crits, " | ")}
      #close brackets after subcondition
      else {crits <- paste0(crits, ")")}
    }
    #create nested list with criteria for each condition of each mids level
    list_criteria[[names(midsschema[sect_index])]][[condition_name]] <- crits
  }
}

list_extra_props <- c("[dwc:Occurrence]dwc:datasetKey",
                      "[dwc:Occurrence]dwc:countryCode",
                      "[dwc:Occurrence]dwc:kingdom",
                      "[dwc:Occurrence]dwc:phylum", 
                      "[dwc:Occurrence]dwc:class",
                      "[dwc:Occurrence]dwc:order", 
                      "[dwc:Occurrence]dwc:family",
                      "[dwc:Occurrence]dwc:subfamily",
                      "[dwc:Occurrence]dwc:genus")

select_props = unique(c(list_props, list_extra_props, "gbifid"))
uom = list_UoM$all

## end of copied code

# remove the class and namespace for the calculation with the tsv file from the sql api
# this file has no metadata and thus no easy way to set those
select_props_truncated = select_props %>%
  gsub(".*:","",.)

## custom function to remove the class and namespace from the filter arguments
## that have been set up by the midscalculator code by default
clean_backtick_texts <- function(x) {
  if (is.list(x)) {
    lapply(x, clean_backtick_texts)
  } else if (is.character(x)) {
    sapply(x, function(str) {
      # Find all substrings enclosed in backticks
      matches <- gregexpr("`[^`]*`", str)[[1]]
      if (matches[1] == -1) return(str)  # No matches, return original
      
      # Extract those substrings
      substrings <- regmatches(str, gregexpr("`[^`]*`", str))[[1]]
      
      # Process each: remove everything before final colon
      cleaned <- sapply(substrings, function(s) {
        content <- sub("^`(.*)`$", "\\1", s)
        trimmed <- sub(".*:", "", content)
        paste0("`", trimmed, "`")
      })
      
      # Replace in original string
      regmatches(str, gregexpr("`[^`]*`", str))[[1]] <- cleaned
      str
    }, USE.NAMES = FALSE)
  } else {
    x
  }
}

list_criteria_truncated = list_criteria %>%
  clean_backtick_texts()

# load pointer to the parquet file
ds <- open_dataset(config$app$parquetpath)

# list the colnames in the parquet file
data_colnames = schema(ds)$names

# select only those used in the SSSOM mapping
ds_props = c(select_props_truncated)[c(select_props_truncated) %in% data_colnames] %>%
  c("gbifID","datasetKey","phylum") %>%
  unique()

# scan only for those columns, do not parallellize and potentially OOM
scan <- Scanner$create(
  dataset = ds,
  projection = ds_props, 
  use_threads = FALSE
)

## start the reader for the parquet file
reader <- as_record_batch_reader(scan)

## define the output schema to be saved as the results parquet file
output_schema <- schema(
  gbifID = int64(),
  datasetKey = string(),
  MIDS_level = int8(),
  `mids:MIDS0PhysicalSpecimenID` = boolean(),
  `mids:MIDS0Organization` = boolean(),
  `mids:MIDS1Name` = boolean(),
  `mids:MIDS1ObjectType` = boolean(),
  `mids:MIDS1License` = boolean(),
  `mids:MIDS1Modified` = boolean(),
  `mids:MIDS2QualitativeLocation` = boolean(),
  `mids:MIDS2QuantitativeLocation` = boolean(),
  `mids:MIDS2CollectingAgent` = boolean(),
  `mids:MIDS2DateCollected` = boolean(),
  `mids:MIDS2CollectingNumber` = boolean(),
  `mids:MIDS2Media` = boolean(),
  `mids:MIDS3GeographicalLocalityID` = boolean(),
  `mids:MIDS3OrganizationID` = boolean(),
  `mids:MIDS3CollectingAgentID` = boolean(),
  `mids:MIDS3IdentifiedAsID` = boolean(),
  `mids:MIDS3IdentifiedByID` = boolean(),
  `mids:MIDS3MediaID`= boolean(),
  year = int16(),
  countryCode = string(),
  phylum = string()
)

## start the output reader for saving the results
output_name = paste0("outputs/dwcFile ",
                     format(Sys.time(), "%Y-%m-%d %I.%M%p"),
                     ".csv")
sink <- FileOutputStream$create(output_name)
writer <- ParquetFileWriter$create(
  schema = output_schema,
  sink = sink,
  properties = ParquetWriterProperties$create(
    column_names = names(output_schema),
    compression = "zstd" # 'zstd' offers elite compression for booleans/integers
  )
)

#add missing columns with all values as NA
list_props %<>% unique()
list_props_truncated = list_props %>%
  gsub(".*:","",.)
missing <- c(list_props_truncated)[!c(list_props_truncated) %in% data_colnames] %>%
  unique()

# loop in batches equal to the parquet's chunk_size
## and calculate mids for each batch

ibig = 1 #progress tracker
while (!is.null(batch <- reader$read_next_batch())) {
  # initial timestamp
  print(paste0("INIT batch ",ibig," at ",Sys.time()))
  ibig = ibig + 1

  #batch <- reader$read_next_batch()
  
  # read the data chunk in memory as a data.table object
  chunk <- as.data.table(batch)
  # add missing columns from the SSSOM mapping, so the calculator code doesn't crash
  chunk[, missing] <- as.character(NA)
  
  ## slightly adapted MIDS calculation code below
  
  # change unknown or missing values for specific columns to NA
  for (i in 1:length(list_UoM)){
    colname <- names(list_UoM[i]) %>%
      gsub(".*:","",.)
    if (colname %in% names(chunk)){
      chunk %<>%
        mutate("{colname}" := na_if(chunk[[colname]], list_UoM[[i]]))
    }
  }
  
  # Check if separate MIDS conditions are met -------------------------------
  
  #For each MIDS condition in the list, check if the criteria for that condition 
  #are TRUE or FALSE and add the results in a new column
  for (j in 1:length(list_criteria_truncated )){
    midslevel <- names(list_criteria_truncated [j])
    midscrit <- list_criteria_truncated [[j]]
    for (i in 1:length(midscrit)){
      columnname = paste0(midslevel,  names(midscrit[i]))
      chunk %<>%
        mutate("{columnname}" := !!rlang::parse_expr(midscrit[[i]]))
    }
  }
  
  # Calculate MIDS level ----------------------------------------------------
  
  #For each MIDS level, the conditions of that level and of lower levels all need to be true
  chunk %<>%
    mutate(MIDS_level = case_when(
      apply(chunk[ , grep("mids:MIDS[0-3]", names(chunk)), with = FALSE], MARGIN = 1, FUN = all) ~ 3,
      apply(chunk[ , grep("mids:MIDS[0-2]", names(chunk)), with = FALSE], MARGIN = 1, FUN = all) ~ 2,
      apply(chunk[ , grep("mids:MIDS[0-1]", names(chunk)), with = FALSE], MARGIN = 1, FUN = all) ~ 1,
      apply(chunk[ , grep("mids:MIDS0", names(chunk)), with = FALSE], MARGIN = 1, FUN = all) ~ 0,
      TRUE ~ -1
    ))
  
  ## end of adapted MIDS calculation code (and end of calculation process)
  
  # convert the data.table to arrow table with only results columns
  raw_table = chunk %>%
    select(all_of(names(output_schema))) %>%
    arrow_table()
  
  # convert to the correct data type in arrow
  output_table <- raw_table$cast(output_schema)
  
  # write to file
  writer$WriteTable(output_table,chunk_size = nrow(output_table))
  
  # remove temporary data objects to keep them from lurking in memory
  rm(chunk,raw_table,output_table,batch)
  
  # garbage collect after every 20 batches
  if (ibig %% 20 == 0) {
    gc(verbose = FALSE) 
  }
  
  # end of iteration timestamp
  print(paste0("FINISH batch ",ibig," at ",Sys.time()))
}

# close the writer pointers
writer$Close()
sink$close()
