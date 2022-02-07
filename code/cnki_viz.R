library(glue)
library(data.table)
library(here)
library(stringr)
library(dplyr)
library(ggplot2)
library(showtext)
library(lubridate)
library(jsonlite)


df <- as.data.table(fromJSON("./out/cnki/sixing_sihuan_guanjianci_counts.json"))[,.(year = name, n = y)]

df[,year := paste0(year, "-01-01")]
df[,year := ymd(year)]
df[,year := floor_date(date, "year")]

df[year > "1990-01-01"] %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(colour = "black", fill = "white") +
  # scale_colour_brewer(palette = "Set1") +
  theme_classic(base_size = 14) +
  theme(text = element_text(family = "mono", color = "black")) +
  scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("1945-01-01"), 
                                             to = as.Date("2020-01-01"), 
                                             by = "5 years"), date_labels = "%y", 
               expand = c(0,0)) +
  xlab("year") +
  ylab("") +
  labs(title = glue("Absolute number of papers in CNKI about death penalty \n(with or without reprieve)"))

ggsave("./out/cnki/absolute_sixing_sihuan_by_year_col.png", device = "png")



df2 <- as.data.table(fromJSON("./out/cnki/xingshi_sifa_guanjianci_counts.json"))[,.(year = name, n = y)]

df2[,year := paste0(year, "-01-01")]
df2[,year := ymd(year)]

df2[year > "1990-01-01" & year < "2022-01-01"] %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(colour = "black", fill = "white") +
  # scale_colour_brewer(palette = "Set1") +
  theme_classic(base_size = 14) +
  theme(text = element_text(family = "mono", color = "black")) +
  scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("1945-01-01"), 
                                             to = as.Date("2020-01-01"), 
                                             by = "5 years"), date_labels = "%y", 
               expand = c(0,0)) +
  xlab("year") +
  ylab("") +
  labs(title = glue("Absolute number of papers in CNKI about criminal justice"))

ggsave("./out/cnki/absolute_sixing_sihuan_by_year_col.png", device = "png")


