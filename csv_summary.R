csvs = list.files("outputs", pattern="*.csv",full.names = T)
current_vals = c(2023,2024,2025,2026)
for (i in 1:length(csvs)) {
  temp = read_tsv(csvs[i])
  temp$year = current_vals[i]
  if (i == 1) {
    resu = temp
  } else {
    resu = rbind(resu,temp)
  }
}

resu_levels = filter(resu,column_name%in%c("0","1","2","3","-1"))
resu_levels %<>%
  mutate(xlab = paste0(year,":",column_name))

ggplot(resu_levels, aes(x = xlab, y = true_count)) + geom_bar(stat = "identity")

ggplot(resu_levels, aes(x = xlab, y = true_percentage)) + geom_bar(stat = "identity")
