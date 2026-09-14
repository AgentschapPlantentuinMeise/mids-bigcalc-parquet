library(tidyverse)
library(magrittr)
csvs = list.files("outputs", pattern="*.csv",full.names = T)
current_vals = seq(2016,2026)
current_vals = current_vals[-2]
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

resu_levels2 <- resu_levels %>%
  separate(xlab, into = c("year", "level"), sep = ":", remove = FALSE) %>%
  mutate(
    year  = as.integer(year),
    level = factor(level, levels = c("-1", "0", "1", "2", "3"))
  ) %>%
  arrange(year, level) %>%
  mutate(xpos = row_number())

ggplot(
  resu_levels2,
  aes(x = level, y = true_percentage, fill = level)
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
    y = "Percentage of GBIF Specimen records",
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

year_info <- resu_levels2 %>%
  group_by(year) %>%
  summarise(
    total = sum(true_count, na.rm = TRUE),
    xmin  = min(xpos),
    xmax  = max(xpos),
    mid   = mean(range(xpos)),
    .groups = "drop"
  )

ggplot(resu_levels2,
       aes(x = xpos, y = true_count/1000000, fill = level)) +
  
  # Counts for individual levels
  geom_col(width = 0.8) +
  
  # Total number of records per year
  geom_line(
    data = year_info,
    aes(x = mid, y = total/1000000),
    inherit.aes = FALSE,
    linewidth = 1,
    group = 1
  ) +
  geom_point(
    data = year_info,
    aes(x = mid, y = total/1000000),
    inherit.aes = FALSE,
    size = 2.5
  ) +
  
  # Separators between years
  geom_vline(
    xintercept = head(year_info$xmax + 0.5, -1),
    colour = "grey70",
    linewidth = 0.5
  ) +
  
  # Level labels
  scale_x_continuous(
    breaks = resu_levels2$xpos,
    labels = resu_levels2$level,
    expand = expansion(add = 0.5)
  ) +
  
  # scale_y_continuous(
  #   labels = scales::label_number(big.mark = ","),
  #   expand = expansion(mult = c(0, 0.05))
  # ) +
  
  labs(
    x = NULL,
    y = "Number of GBIF Specimen records (Millions)",
    fill = "Level"
  ) +
  
  # Year labels underneath the level labels
  geom_text(
    data = year_info,
    aes(x = mid, y = -Inf, label = year),
    inherit.aes = FALSE,
    vjust = 3.2,
    fontface = "bold",
    size = 24/.pt
  ) +
  
  theme_minimal(base_size = 24) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    
    # Extra room for year labels
    plot.margin = margin(5.5, 5.5, 25, 5.5)
  ) +
  
  coord_cartesian(clip = "off")
