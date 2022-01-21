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

# font_install(source_han_serif())
# font_families()
here <- rprojroot::find_rstudio_root_file()

`%notlike%` <- Negate(`%like%`)
`%notchin%` <- Negate(`%chin%`)

con <- dbConnect(RSQLite::SQLite(), "/Volumes/2tb_nvme_a/projects/peoples_daily_database/data/peoples_daily.sqlite")

dt <- as.data.table(dbReadTable(con, "peoples_daily"))
dt1 <- dt
dt1[,fulltext := str_remove_all(fulltext, "\\n|\\r|\\t|\\s+")]

options(datatable.prettyprint.char=20L)

dt1[,date := ymd(date)]
dt1[,month := floor_date(date, "month")]
dt1[,day := floor_date(date, "day")]
dt1[,year := floor_date(date, "year")]
dt1[,decade := as.numeric(year(year)) %/% 10 * 10]

ft_nchar_month <- dt1[,.(ft_nchar_sum = sum(ft_nchar)), by = month]
ft_nchar_year <- dt1[,.(ft_nchar_sum = sum(ft_nchar)), by = year]
ft_nchar_decade <- dt1[,.(ft_nchar_sum = sum(ft_nchar)), by = decade]
page_one_headlines_month <- dt1[page_num == 1, .(n_headline = .N), by = month]
page_one_headlines_year <- dt1[page_num == 1, .(n_headline = .N), by = year]
page_one_headlines_decade <- dt1[page_num == 1, .(n_headline = .N), by = decade]

dp_in_hed <- dt1[headline %like% "死刑"]
dp_in_fulltext <- dt1[fulltext %like% "死刑"]
sihuan_in_hed <- dt1[headline %like% "死缓"]
sihuan_in_fulltext <- dt1[fulltext %like% "死缓"]





# Tokenize ----------------------------------------------------------------

cutter = worker()

GetTokens <- function(fulltext){
  a <- segment(fulltext, cutter)
  return(paste(a, collapse = " "))
}
dp_in_fulltext[,fulltext := str_remove_all(fulltext, "\\n|\\r|\\t|\\s+")]
tic()
dp_in_fulltext[, ft_toks := map_chr(fulltext, ~GetTokens(.x))]
toc()

corp <- corpus(dp_in_fulltext[,-c("fulltext")], docid_field = "docid", text_field = "ft_toks")

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



dp_in_hed %>%
  count(year) |> 
  ggplot(aes(x = month, y = n)) +
  geom_col() +
  theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("month") +
  ylab("") +
  labs(title = glue("Absolute number of headlines with death penalty by year")) +
  theme(plot.title = element_textbox_simple(
    size = 13,
    lineheight = 1,
    padding = margin(5.5, 5.5, 5.5, 5.5),
    margin = margin(0, 0, 5.5, 0)
  ))

dp_in_fulltext %>%
  count(year) |>  
  ggplot(aes(x = year, y = n)) +
  geom_col() +
  theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("year") +
  ylab("") +
  labs(title = glue("Absolute number of articles that mention 'death penalty' by year")) 







