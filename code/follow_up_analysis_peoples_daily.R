library(glue)
library(data.table)
library(RSQLite)
library(here)
library(purrr)
library(stringi)
library(lubridate)
library(quanteda)
library(quanteda.corpora)
library(seededlda)
library(jiebaR)
library(tidyverse)
library(tictoc)
here <- rprojroot::find_rstudio_root_file()
`%notlike%` <- Negate(`%like%`)
`%notchin%` <- Negate(`%chin%`)
options(datatable.prettyprint.char=20L)

dt <- fread("./data/peoples_daily/peoples_daily_sixing_sihuan_ft_title.csv")

dt[,date := ymd(date)]
dt[,month := floor_date(date, "month")]
dt[,day := floor_date(date, "day")]
dt[,year := floor_date(date, "year")]
dt[,decade := as.numeric(year(year)) %/% 10 * 10]

dt[year == "2012-01-01",.(docid, headline, subhead, section, date, fulltext)] %>% fwrite("./out/peoples_daily/pd_2012_inspect.csv")

cutter <- jiebaR::worker(type = "mix")
dt[, toks := map(fulltext, ~segment(.x, cutter))]
# dt[, toks := map(toks, ~as.tokens(as.list(paste(.x, collapse = " "))))]

dt_corp <- corpus(dt, text_field = "fulltext")
# Tried and failed to implement the jiebaR tokenizer. Just using regular quanteda tokenizer. 
# toks_dt_corp <- map(dt_corp, ~as.list(paste(segment(.x, cutter), collapse = " ")) %>% as.tokens(.))
# map(toks_dt_corp, ~kwic(.x, "死刑"))

dt_corp %>% corpus_subset(subset = year == "2012-01-01") %>% 
  tokens() %>% 
  tokens_keep("死刑") %>%
  dfm() %>% convert(., to = "data.frame")

kwic(dt_toks, "死刑")