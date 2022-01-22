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
# dt1 |> fwrite("./data/peoples_daily/peoples_daily_sixing_sihuan_ft_title.csv")
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
# ft_nchar_month |> fwrite("./data/peoples_daily_ft_nchar_month.csv")
# ft_nchar_year |> fwrite("./data/peoples_daily_ft_nchar_year.csv")
# ft_nchar_decade |> fwrite("./data/peoples_daily_ft_nchar_decade.csv")
# page_one_headlines_month |> fwrite("./data/peoples_daily_page_one_headlines_month.csv")
# page_one_headlines_year |> fwrite("./data/peoples_daily_page_one_headlines_year.csv")
# page_one_headlines_decade |> fwrite("./data/peoples_daily_page_one_headlines_decade.csv")
# total_pubs_month |> fwrite("./data/peoples_daily_total_pubs_month.csv")
# total_pubs_year |> fwrite("./data/peoples_daily_total_pubs_year.csv")
  
# DONE --------------------------------------------------------------------

dt1 <- fread("./data/peoples_daily/peoples_daily_sixing_sihuan_ft_title.csv")

dt1[,date := ymd(date)]
dt1[,month := floor_date(date, "month")]
dt1[,day := floor_date(date, "day")]
dt1[,year := floor_date(date, "year")]
dt1[,decade := as.numeric(year(year)) %/% 10 * 10]

ft_nchar_month  <-  fread("./data/peoples_daily_ft_nchar_month.csv")
ft_nchar_year  <-  fread("./data/peoples_daily_ft_nchar_year.csv")
ft_nchar_decade  <-  fread("./data/peoples_daily_ft_nchar_decade.csv")
page_one_headlines_month  <-  fread("./data/peoples_daily_page_one_headlines_month.csv")
page_one_headlines_year  <-  fread("./data/peoples_daily_page_one_headlines_year.csv")[,total_n := n_headline][,-c("n_headline")][,year := floor_date(year, "year")]
page_one_headlines_decade  <-  fread("./data/peoples_daily_page_one_headlines_decade.csv")
total_pubs_month <- fread("./data/peoples_daily_total_pubs_month.csv")[,total_n := n][,-c("n")][,month := floor_date(month, "month")]
total_pubs_year  <- fread("./data/peoples_daily_total_pubs_year.csv")[,total_n := n][,-c("n")][,year := floor_date(year, "year")]


dp_sh_hed <- dt1[!is.na(year) & headline %like% "死刑|死缓"]
dp_sh_fulltext <- dt1[!is.na(year) & fulltext %like% "死刑|死缓"]

# Absolute number of mentions with average line by year ---------------------------
dp_sh_fulltext %>%
  count(year) |> 
  mutate(mean_count = mean(n)) |> 
  ggplot(aes(x = year, y = n)) +
  geom_col() +
  # scale_colour_brewer(palette = "Set1") +
  geom_hline(aes(color = "red", yintercept = mean_count), show.legend = F, size = 1) +
  theme_classic(base_size = 14) +
  theme(text = element_text(family = "", color = "black")) +
  scale_y_continuous(labels = scales::comma) +
  xlab("year") +
  ylab("") +
  labs(title = glue("Absolute number of articles that mention death penalty (with or without reprieve) by year"))

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
  theme(text = element_text(family = "", color = "black")) +
  scale_y_continuous(labels = scales::percent) +
  xlab("year") +
  ylab("") +
  labs(title = glue("Percent of articles in People's Daily that mention death penalty (with or without reprieve) by year"))

#  Percent of page one articles that mention death penalty per year ---------------------------

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
  theme(text = element_text(family = "", color = "black")) +
  scale_y_continuous(labels = scales::percent) +
  xlab("year") +
  ylab("") +
  labs(title = glue("Percent of front page articles in People's Daily that mention death penalty (with or without reprieve) by year"))



# "Absolute number of articles in People's Daily that mention death penalty by month --------
dp_sh_fulltext[month > "2012-01-01"] |> 
  count(month) |> 
  ggplot(aes(x = month, y = n)) +
  geom_line() +
  theme_classic(base_size = 14) +
  theme(text = element_text(family = "", color = "black")) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_brewer(palette = "Dark2") +
  xlab("year") +
  ylab("") +
  labs(title = glue("Absolute number of articles in People's Daily that mention death penalty (with or without reprieve) by month"))



# TOPIC MODELS! -----------------------------------------------------------

# Tokenize ----------------------------------------------------------------

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

tmod_lda <- textmodel_lda(pd_dfm, k = 5)

terms(tmod_lda, 30)

head(topics(tmod_lda), 20)


# assign topic as a new document-level variable
dfmat_news$topic <- topics(tmod_lda)

# cross-table of the topic frequency
table(dfmat_news$topic)





View(pd_dfm)









corp_immig <- corpus(data_char_ukimmig2010, 
                     docvars = data.frame(party = names(data_char_ukimmig2010)))
print(corp_immig)
str(corp_immig)
View(data_char_ukimmig2010)
str(data_char_ukimmig2010)


testout[docid == "201303_2243"]$ft_toks



