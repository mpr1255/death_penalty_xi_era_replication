# Exploratory code for an analysis of what is going on in the People's Daily death penalty-related reports between 2013 and 2015 (inclusive.)

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
library("quanteda.textstats")
library(showtext)
showtext_auto()

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

cutter <- jiebaR::worker(type = "mix")
dt[, toks := map(fulltext, ~segment(.x, cutter))]
# dt[, toks := map(toks, ~as.tokens(as.list(paste(.x, collapse = " "))))]

dt_corp <- corpus(dt, text_field = "fulltext")
dt_corp_as_dt <- docvars(dt_corp) %>% as.data.table() 


# Tried and failed to implement the jiebaR tokenizer. Just using regular quanteda tokenizer. 
# toks_dt_corp <- map(dt_corp, ~as.list(paste(segment(.x, cutter), collapse = " ")) %>% as.tokens(.))
# map(toks_dt_corp, ~kwic(.x, "死刑"))

dp_counts <- dt_corp %>% 
  tokens() %>% 
  tokens_keep("死刑") %>%
  dfm() %>% 
  convert(., to = "data.frame") %>% 
  cbind(dt_corp_as_dt) %>% 
  as.data.table()

dp_counts[dt, on = "docid"][,.(doc_id, 死刑, docid, headline, subhead, section, date, page_num, month, year, fulltext)] %>%
  filter(year == "2012-01-01") %>% 
  arrange(date) %>% 
  fwrite("./out/peoples_daily/pd_2012_inspect.csv")
  

dp_counts[dt, on = "docid"][,.(doc_id, 死刑, docid, headline, subhead, section, date, page_num, month, year, fulltext)] %>%
  filter(year > "2012-01-01") %>% 
  arrange(date) %>% 
  fwrite("./out/peoples_daily/pd_2013_onwards_inspect.csv")


dp_counts %>% 
  filter(date > "2010-01-01" & nchar(section) > 1) %>% 
  count(section, year) %>% 
  filter(n > 2) %>% 
  group_by(section) %>% 
  filter(n() > 2) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = n, color = section)) +
  geom_line() +
  facet_wrap("section", scales = "free_y") 


dp_counts %>% 
  filter(date > "2010-01-01" & page_num < 20) %>% 
  count(page_num, year) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  facet_wrap("page_num", scales = "free_y") +
  theme_bw()



dp_counts[month > "2010-01-01"] %>% 
  ggplot(aes(x = month, y = 死刑)) +
  geom_col() +
  theme_classic(base_size = 14)
  theme(text = element_text(family = "mono", color = "black")) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_brewer(palette = "Dark2") +
  xlab("year") +
  ylab("") +
  labs(title = glue("Absolute number of articles in People's Daily that mention death penalty \n(with or without reprieve) by month"))

# A useful way of figuring out what is going on in these publications from 2012-2020 might be to simply pull the keywords in context. So we identify 30 words EITHER SIDE of the mention of 死刑, then look at the relative frequency of those surrounding terms for each year and see if there are any patterns. This would also allow looking at the most frequent words for each year that surround sixing. 
kwic_res <- kwic(dt_toks, pattern = "死刑|死缓", valuetype = "regex", window = 50) %>% 
  as.data.table() %>% 
  rename(doc_id = docname)

kwic_res <- kwic_res[dp_counts, on = "doc_id"] %>% 
  mutate(doc_id_original = doc_id, 
         index = row_number())

kwic_res[,combined := paste(pre, post)]

# This captures the kwic surrounding terms but in a peculiar way -- i.e. only terms that have been in there for the 10 year period. 
kwic_res[year > "2010-01-01"] %>% 
  corpus(text_field = "combined", unique_docnames = F) %>% 
  tokens(remove_symbols = T, remove_punct = T, remove_numbers = T) %>% 
  tokens_remove(stopwords(language = "zh", source = "misc")) %>%
  tokens_remove(c("死刑")) %>%
  tokens_select(min_nchar = 2) %>% 
  dfm() %>% 
  textstat_frequency(n = 100, groups = year, force = T) %>% 
  as_tibble() %>% 
  mutate(group = ymd(group)) %>% 
  group_by(feature) %>% 
  filter(n()>10) %>% 
  ggplot(aes(x = group, y = frequency, color = feature)) +
  geom_line(show.legend = F) +
  facet_wrap("feature", scales = "free_y") +
  theme_bw()

# But if we just want to get those that have been most prevalent in the most recent years, we have to filter it differently.... 
kwic_intermediary <- kwic_res[year > "2000-01-01"] %>% 
  corpus(text_field = "combined", unique_docnames = F) %>% 
  tokens(remove_symbols = T, remove_punct = T, remove_numbers = T) %>% 
  tokens_remove(stopwords(language = "zh", source = "misc")) %>%
  tokens_remove(c("死刑")) %>%
  tokens_select(min_nchar = 2) %>% 
  dfm() %>% 
  textstat_frequency(n = 100, groups = year, force = T) %>% 
  as.data.table()

kwic_intermediary %>% 
  filter(feature %chin% kwic_intermediary[group > "2020-01-01" & rank < 20]$feature) %>% 
  mutate(group = ymd(group)) %>% 
  group_by(feature) %>% 
  filter(n()>10) %>% 
  ggplot(aes(x = group, y = frequency, color = feature)) +
  geom_line(show.legend = F) +
  facet_wrap("feature", scales = "free_y") +
  theme_bw()

kwic_intermediary[feature %chin% kwic_intermediary[group > "2020-01-01" & rank < 100]$feature & feature %notchin% kwic_intermediary[group < "2020-01-01" & rank < 100]$feature][order(desc(frequency))]

kwic_res[year == "2012-01-01"] %>% 
  corpus(text_field = "combined", unique_docnames = F) %>% 
  tokens(remove_symbols = T, remove_punct = T) %>% 
  tokens_remove(stopwords(language = "zh", source = "misc")) %>%
  tokens_remove(c("死刑")) %>%
  tokens_select(min_nchar = 2) %>% 
  dfm() %>% 
  textstat_frequency(n = 10, ) %>% 
  ggplot(aes(x = reorder(feature, frequency),
             y = frequency)) +
  geom_point() +
  coord_flip() +
  theme_bw(base_family = "wqy-microhei") +
  labs(x = NULL, y = "Frequency")

kwic_res[year == "2014-01-01"] %>% 
  corpus(text_field = "combined", unique_docnames = F) %>% 
  tokens(remove_symbols = T, remove_punct = T, remove_numbers = T) %>% 
  tokens_remove(stopwords(language = "zh", source = "misc")) %>%
  tokens_remove(c("死刑")) %>%
  tokens_select(min_nchar = 2) %>% 
  dfm() %>% 
  textstat_frequency(n = 10, ) %>% 
  ggplot(aes(x = reorder(feature, frequency),
             y = frequency)) +
  geom_point() +
  coord_flip() +
  theme_bw(base_family = "wqy-microhei") +
  labs(x = NULL, y = "Frequency")



# Check on some of the very specific questions Sue had about the content of the 2013-2015 publications. 

nrow(dt[year >= "2013-01-01" & year <= "2015-01-01" & fulltext %like% "修正|腐败|反腐|冤假错案|无期徒刑|刑事诉讼法"]) / nrow(dt[year >= "2013-01-01" & year <= "2015-01-01"]) 

dt[year >= "2013-01-01" & year <= "2015-01-01" & fulltext %notlike% "修正|腐败|反腐|冤假错案|无期徒刑|刑事诉讼法", .(sum = .N), by = "year"]


dt[year >= "2013-01-01" & year <= "2015-01-01" & fulltext %notlike% "修正|腐败|反腐|冤假错案|无期徒刑|刑事诉讼法", .N, by = "section"]

dt[year >= "2013-01-01" & year <= "2015-01-01" & fulltext %notlike% "修正|腐败|反腐|冤假错案|无期徒刑|刑事诉讼法"]

37/193

dt[year >= "2013-01-01" & year <= "2015-01-01" & fulltext %like% "修正|腐败|反腐|冤假错案|无期徒刑|刑事诉讼法"]


# Just looking at  distribution of front pages
dt %>% count(page_num, year) %>% filter(page_num == 1) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_col()