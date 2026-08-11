library(arrow)

parquet_files = list.files("outputs",pattern="*.parquet$",full.names=T)

results = tibble(file=as.character(NULL),n=as.numeric(NULL))
for (i in parquet_files) {
  pointer = open_dataset(i)
  results = rbind(results,c(i,dim(pointer)[1]))
}
colnames(results) = c("file","n")

write_tsv(results,"outputs/numbers_of_specimens.txt")
