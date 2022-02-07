library(glue)
library(data.table)
library(RSQLite)
library(here)
library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(stringi)
library(ggtext)
library(ggplot2)
library(showtext)
library(lubridate)
library(pinyin)
require(quanteda)
require(quanteda.corpora)
require(seededlda)
library(jiebaR)
library(tidyr)
library(furrr)
library(tictoc)
library(extrafont)
font_import(paths = "/Library/Fonts/") # import all your fonts
font_import(pattern = "Garamond.ttf")
fonts() #get a list of fonts
fonttable()
# fonttable()[40:45,] 
library(extrafontdb)


# font_install(source_han_serif())
# font_families()
here <- rprojroot::find_rstudio_root_file()

`%notlike%` <- Negate(`%like%`)
`%notchin%` <- Negate(`%chin%`)
options(datatable.prettyprint.char=20L)


# RAN ONCE ONLY -----------------------------------------------------------
# Did this for speedups on reruns. 
# This is how the data table was created, from the full corpus ------------
# con <- dbConnect(RSQLite::SQLite(), "/Volumes/2tb_nvme_a/projects/peoples_daily_database/data/peoples_daily.sqlite")
# dt <- as.data.table(dbReadTable(con, "peoples_daily"))
# dt1 <- dt[headline %like% "死刑|死缓" | fulltext %like% "死刑|死缓"]
# dt1[,fulltext := str_remove_all(fulltext, "\\n|\\r|\\t|\\s+")]
# dt1 |> fwrite("./data/peoples_daily/peoples_daily/peoples_daily_sixing_sihuan_ft_title.csv")
# dt1 <- dt
# dt1[,date := ymd(date)]
# dt1[,month := floor_date(date, "month")]
# dt1[,day := floor_date(date, "day")]
# dt1[,year := floor_date(date, "year")]
# dt1[,decade := as.numeric(year(year)) %/% 10 * 10]
# ft_nchar_month <- dt1[,.(ft_nchar_sum = sum(ft_nchar)), by = month]
# ft_nchar_year <- dt1[,.(ft_nchar_sum = sum(ft_nchar)), by = year]
# ft_nchar_decade <- dt1[,.(ft_nchar_sum = sum(ft_nchar)), by = decade]
# page_one_headlines_month <- dt1[page_num == 1, .(n_headline = .N), by = month]
# page_one_headlines_year <- dt1[page_num == 1, .(n_headline = .N), by = year]
# page_one_headlines_decade <- dt1[page_num == 1, .(n_headline = .N), by = decade]
# total_pubs_month <- dt1[,n := .N, by = "month"][!is.na(month),.(month, n)] |> distinct(month, .keep_all = T)
# total_pubs_year <- dt1[,n := .N, by = "year"][!is.na(year),.(year, n)] |> distinct(year, .keep_all = T)
# 
# ft_nchar_month |> fwrite("./data/peoples_daily/peoples_daily_ft_nchar_month.csv")
# ft_nchar_year |> fwrite("./data/peoples_daily/peoples_daily_ft_nchar_year.csv")
# ft_nchar_decade |> fwrite("./data/peoples_daily/peoples_daily_ft_nchar_decade.csv")
# page_one_headlines_month |> fwrite("./data/peoples_daily/peoples_daily_page_one_headlines_month.csv")
# page_one_headlines_year |> fwrite("./data/peoples_daily/peoples_daily_page_one_headlines_year.csv")
# page_one_headlines_decade |> fwrite("./data/peoples_daily/peoples_daily_page_one_headlines_decade.csv")
# total_pubs_month |> fwrite("./data/peoples_daily/peoples_daily_total_pubs_month.csv")
# total_pubs_year |> fwrite("./data/peoples_daily/peoples_daily_total_pubs_year.csv")
  
# DONE --------------------------------------------------------------------

dt1 <- fread("./data/peoples_daily/peoples_daily_sixing_sihuan_ft_title.csv")

dt1[,date := ymd(date)]
dt1[,month := floor_date(date, "month")]
dt1[,day := floor_date(date, "day")]
dt1[,year := floor_date(date, "year")]
dt1[,decade := as.numeric(year(year)) %/% 10 * 10]


# Mean
dt1 |> 
  count(year) |> 
  drop_na() |> 
  summarise(mean = mean(n))

# SD
dt1 |> 
  count(year) |> 
  drop_na() |> 
  summarise(sd = sd(n))

# variance
dt1 |> 
  count(year) |> 
  drop_na() |> 
  summarise(var = var(n))


dt1 |> 
  count(year) |> 
  drop_na() |> 
  ggplot() +
  geom_histogram(aes(n), bins = 35) +
  theme_classic()


ft_nchar_month  <-  fread("./data/peoples_daily/peoples_daily_ft_nchar_month.csv")
ft_nchar_year  <-  fread("./data/peoples_daily/peoples_daily_ft_nchar_year.csv")
ft_nchar_decade  <-  fread("./data/peoples_daily/peoples_daily_ft_nchar_decade.csv")
page_one_headlines_month  <-  fread("./data/peoples_daily/peoples_daily_page_one_headlines_month.csv")
page_one_headlines_year  <-  fread("./data/peoples_daily/peoples_daily_page_one_headlines_year.csv")[,total_n := n_headline][,-c("n_headline")][,year := floor_date(year, "year")]
page_one_headlines_decade  <-  fread("./data/peoples_daily/peoples_daily_page_one_headlines_decade.csv")
total_pubs_month <- fread("./data/peoples_daily/peoples_daily_total_pubs_month.csv")[,total_n := n][,-c("n")][,month := floor_date(month, "month")]
total_pubs_year  <- fread("./data/peoples_daily/peoples_daily_total_pubs_year.csv")[,total_n := n][,-c("n")][,year := floor_date(year, "year")]


dp_sh_hed <- dt1[!is.na(year) & headline %like% "死刑|死缓"]
dp_sh_fulltext <- dt1[!is.na(year) & fulltext %like% "死刑|死缓"]

# Absolute number of mentions with average line by year ---------------------------

dp_sh_fulltext %>%
  count(year) |> 
  mutate(mean_count = mean(n)) |> 
  ggplot(aes(x = year, y = n)) +
  geom_col(colour = "black", fill = "white") +
  # scale_colour_brewer(palette = "Set1") +
  geom_hline(aes(color = "red", yintercept = mean_count), show.legend = F, size = 1) +
  theme_classic(base_size = 14) +
  theme(text = element_text(family = "mono", color = "black")) +
  scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("1945-01-01"), 
                                             to = as.Date("2020-01-01"), 
                                             by = "5 years"), date_labels = "%y", 
               expand = c(0,0)) +
  xlab("year") +
  ylab("") +
  labs(title = glue("Absolute number of articles in People's Daily that mention death penalty \n(with or without reprieve) by year"))

ggsave("./out/peoples_daily/absolute_by_year_col.png", device = "png")


dp_sh_fulltext[year >= "2013-01-01"] %>%
  count(year) |> 
  mutate(mean_count = mean(n)) |> 
  ggplot(aes(x = year, y = n)) +
  geom_col(colour = "black", fill = "white") +
  # scale_colour_brewer(palette = "Set1") +
  geom_hline(aes(color = "red", yintercept = 73.8), show.legend = F, size = 1) +
  theme_classic(base_size = 14) +
  theme(text = element_text(family = "mono", color = "black")) +
  scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2012-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 years"), date_labels = "%y", 
               expand = c(0,0)) +
  xlab("year") +
  ylab("") +
  labs(title = glue("Absolute number of articles in People's Daily in Xi era \nthat mention death penalty (with or without reprieve) by year"))

ggsave("./out/peoples_daily/absolute_by_year_col_xi_era.png", device = "png")

#  Number of mentions as % of total articles with average line by year ---------------------------
dp_sh_fulltext |> 
  count(year) |> 
  left_join(total_pubs_year, by = "year") |>
  mutate(prop = n/total_n,
         mean_prop = mean(prop)) |> 
  ggplot(aes(x = year, y = prop)) +
  geom_col() +
  # scale_colour_brewer(palette = "Set1") +
  geom_hline(aes(color = "red", yintercept = mean_prop), show.legend = F, size = 1) +
  theme_classic(base_size = 14) +
  theme(text = element_text(family = "mono", color = "black")) +
  scale_y_continuous(labels = scales::percent) +
  xlab("year") +
  ylab("") +
  labs(title = glue("Percent of articles in People's Daily that mention death penalty \n(with or without reprieve) by year"))

ggsave("./out/peoples_daily/all_by_percent.png", device = "png")

#  Percent of page one articles that mention death penalty \n per year ---------------------------

dp_sh_hed |> 
  count(year) |> 
  left_join(page_one_headlines_year, by = "year") |> 
  mutate(prop = n/total_n,
         mean_prop = mean(prop)) |> 
  ggplot(aes(x = year, y = prop)) +
  geom_col() +
  scale_colour_brewer(palette = "Set1") +
  # geom_hline(aes(color = "red", yintercept = mean_prop), show.legend = F, size = 1) +
  theme_classic(base_size = 14) +
  theme(text = element_text(family = "mono", color = "black")) +
  scale_y_continuous(labels = scales::percent) +
  xlab("year") +
  ylab("") +
  labs(title = glue("Percent of front page articles in People's Daily that mention death penalty \n(with or without reprieve) by year"))

ggsave("./out/peoples_daily/front_page_by_percent.png", device = "png")

# "Absolute number of articles in People's Daily that mention death penalty \n by month --------
dp_sh_fulltext[month > "2012-01-01"] |> 
  count(month) |> 
  ggplot(aes(x = month, y = n)) +
  geom_line() +
  theme_classic(base_size = 14) +
  theme(text = element_text(family = "mono", color = "black")) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_brewer(palette = "Dark2") +
  xlab("year") +
  ylab("") +
  labs(title = glue("Absolute number of articles in People's Daily that mention death penalty \n(with or without reprieve) by month"))

ggsave("./out/peoples_daily/absolute_by_month.png", device = "png")


######## STOP HERE FOR DESCRIPTIVES ##########










# TOPIC MODELS! -----------------------------------------------------------

# Tokenize ----------------------------------------------------------------

pd_dfm <- read_rds("./out/peoples_daily/pd_dfm.Rds")

if(!exists("pd_dfm")){
  cutter = worker()

  GetTokens <- function(fulltext){
    a <- segment(fulltext, cutter)
    return(paste(a, collapse = " "))
  }
  dp_sh_fulltext[,fulltext := str_remove_all(fulltext, "\\n|\\r|\\t|\\s+")]
  tic()
  dp_sh_fulltext[, ft_toks := map_chr(fulltext, ~GetTokens(.x))]
  toc()
  
  corp <- corpus(dp_sh_fulltext[,-c("fulltext")], docid_field = "docid", text_field = "ft_toks")
  
  toks <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE, padding = F, what = "fasterword") |> 
    tokens_remove(pattern = stopwords("zh_cn", source = "marimo"), padding = F)
  
  pd_dfm <- dfm(toks) %>% 
    dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
             max_docfreq = 0.1, docfreq_type = "prop")

  pd_dfm |> write_rds("./out/peoples_daily/pd_dfm.Rds")
}

# Making four topic models 
# 1. Mao era until 1978 (first yr of reforms)
# 2. 01/1978 to 01/2003 (reform era)
# 3. 01/2003-01/2013 (hu wen era)
# 4. 01/2013-01/2022 ( xi era)

dfm_mao_era <- dfm_subset(pd_dfm, date < "1978-01-01") 
dfm_reform_era <- dfm_subset(pd_dfm, date > "1978-01-01" & date < "2003-01-01")
dfm_hu_wen_era <- dfm_subset(pd_dfm, date > "2003-01-01" & date < "2013-01-01")
dfm_xi_era <- dfm_subset(pd_dfm, date > "2013-01-01" & date < "2022-01-01")

lda1 <- read_rds("./out/peoples_daily/lda1.Rds")
lda2 <- read_rds("./out/peoples_daily/lda2.Rds")
lda3 <- read_rds("./out/peoples_daily/lda3.Rds")
lda4 <- read_rds("./out/peoples_daily/lda4.Rds")

# lda1 <- textmodel_lda(dfm_mao_era, k = 4)
# lda2 <- textmodel_lda(dfm_reform_era, k = 4)
# lda3 <- textmodel_lda(dfm_hu_wen_era, k = 4)
# lda4 <- textmodel_lda(dfm_xi_era, k = 4)
# lda1 |> write_rds("./out/peoples_daily/lda1.Rds")
# lda2 |> write_rds("./out/peoples_daily/lda2.Rds")
# lda3 |> write_rds("./out/peoples_daily/lda3.Rds")
# lda4 |> write_rds("./out/peoples_daily/lda4.Rds")

terms(lda1, 20) |> as.data.table() |> fwrite("./out/peoples_daily/topic_model_20_terms_mao_era.csv")
terms(lda2, 20) |> as.data.table() |> fwrite("./out/peoples_daily/topic_model_20_terms_reform_era.csv")
terms(lda3, 20) |> as.data.table() |> fwrite("./out/peoples_daily/topic_model_20_terms_hu_wen_era.csv")
terms(lda4, 20) |> as.data.table() |> fwrite("./out/peoples_daily/topic_model_20_terms_xi_era.csv")
