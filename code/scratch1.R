library(devtools)
library(readtext)
library(quanteda)
library(quanteda.textplots)
library(quanteda.corpora)
library(quanteda.textstats)
library(tidyverse)
library(utf8)
library(showtext)
library(data.table)
library(stopwords)
library(lubridate)


`%notlike%` <- Negate(`%like%`)

rm(list = ls())

theme_set(theme_light())

##############################################






#################### kohei's code

txt <- "10月初，聯合國軍逆轉戰情，向北開進，越過38度線，終促使中华人民共和国決定出兵介入，中国称此为抗美援朝。"
lis <- list(抗美援朝 = "抗美援朝", 向北開進 = "向北開進")

## tokenize dictionary values
lis <- map(lis, ~stri_c_list(as.list(tokens(.x)), sep = " "))
dict <- dictionary(lis)

## tokenize texts and count
toks <- tokens(txt)
dfm(tokens_lookup(toks, dict))










summary(pd.corp1)


sp <- spacy_parse(pd.corp1, lemma = TRUE, entity = FALSE, pos = FALSE)

sp$token <- sp$lemma

as.tokens(sp) %>% dfm()




pd.toks <- spacy_tokenize(pd.corp1) %>%
  as.tokens() 




get_term_freq <- function(term){
  pd.toks %>%  
    tokens_compound(pattern = phrase("重罪 轻判"), concatenator = " ") %>% 
    tokens_select("重罪 轻判") %>% 
    dfm() %>%
    textstat_frequency()
  
}


step1 <- pd.toks %>%  
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(pattern = ch_stop) %>% 
  # tokens_compound(pattern = phrase("重罪 轻判"), concatenator = " ") %>% 
  # tokens_select("重罪 轻判") %>% 
  dfm() 


step1a <- dfm(pd.toks) 

step2 <- textstat_frequency(step1, groups = year)


ch_toks <- pd.corp1 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(pattern = ch_stop)

dp_dfm <- dfm(ch_toks)























font_add_google("Noto Serif SC", "chinese")

# path_data <- system.file("/mnt/c/Users/m/projects/death_penalty_xi_era/data/txt/", package = "readtext")
# my_corpus <- readtext("/mnt/c/Users/m/projects/death_penalty_xi_era/data/txt/")
# df_files_metadata <- read_csv("/mnt/c/Users/m/projects/peoples_daily_database/data/df_files_metadata.csv")
# df_files_metadata <- df_files_metadata %>% mutate(fulltext = map_chr(path, ~read_file(.x)))
# df_files_metadata %>% fwrite("./data/df_files_metadata_w_fulltext.csv")

df_files_metadata_w_fulltext <- fread("./data/df_files_metadata_w_fulltext.csv")
df_files_metadata_w_fulltext[,year := str_extract(date, "^[0-9]{4}")]
dp_corpus <- corpus(df_files_metadata_w_fulltext, text_field = "fulltext")
ch_stop <- read_lines("./data/stopwords_zh.txt")


corp_dp_in_hed <- corpus_subset(dp_corpus, headline %like% "死刑")

# tokenize
ch_toks <- dp_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(pattern = ch_stop)

dp_dfm <- dfm(ch_toks)

dp_dfm

tstat_freq <- textstat_frequency(dp_dfm, groups = year)

dt_tstat_freq <- as.data.table(tstat_freq)

dt_tstat_freq[feature %like% "少"]
#########################################

dpcorpustest <- corpus("　2015年，刑法修正案（九）决定在贪腐犯罪中引入终身监禁。今年4月，最高法、最高检又联合发布司法解释，明确了对重大贪污受贿罪犯可以决定终身监禁；终身监禁一 旦作出，就不受执行期间服刑表现的影响。这一制度的适用有效填补了死刑立即执行过重而死刑缓期执行过轻之间的落差，避免了贪官钻以往法律制度的空子，从刑事立法和司法的制度层面彻底“补漏”。同时，该制度保障了对贪腐行为施以“牢底坐穿”这一至为严厉的自由刑的法律威慑力，遵循了“少杀慎杀”的刑事司法政策，实现了严厉惩治腐败和严格限制死刑的双重理念的有机结合，是法治反腐的一大进步。")

ch_toks_test <- dpcorpustest %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(pattern = ch_stop)

dp_dfmtest <- dfm(ch_toks_test)

tstat_freqtest <- textstat_frequency(dp_dfmtest)

dt_tstat_freqtest <- as.data.table(tstat_freqtest)

dt_tstat_freqtest[feature %like% "杀"]


# now try ngrams of 2-4 length

chtoks_ngram <- tokens_ngrams(ch_toks, n = 2:4)

dfmat_chtoks_ngram <- dfm(chtoks_ngram)

tstat_freqtest_ngram1 <- textstat_frequency(dfmat_chtoks_ngram, groups = year)

dfmat_chtoks_ngram[headline %like% "死刑"]



dt_tstat_freqtest_ngram <- as.data.table(tstat_freqtest_ngram)


shaosha <- dt_tstat_freqtest_ngram[feature %like% "^少_杀"][frequency > 1] %>% as_tibble()

shaosha_by_year <- shaosha %>% group_by(group) %>% summarise(count = sum(frequency))

shaosha_by_year %>% 
  ggplot(aes(as.numeric(group), as.numeric(count)), group = 1) + 
  geom_line()


##########################

docvars(dfmat_chtoks_ngram)

panchu_sixing <- dt_tstat_freqtest_ngram[feature %like% "^判处_死刑$"][frequency > 1] %>% as_tibble()

panchu_sixing_by_year <- panchu_sixing %>% group_by(group) %>% summarise(count = sum(frequency))

panchu_sixing_by_year %>% 
  ggplot(aes(as.numeric(group), as.numeric(count)), group = 1) + 
  geom_line()





##################################################

dt_for_graph <- df_files_metadata_w_fulltext[,-c("fulltext")] %>% as_tibble()



#####################
dt_for_graph1 <- dt_for_graph %>% 
  mutate(sixing_in_hed = case_when(
    str_detect(headline, "死刑") ~ 1),
    sixing_not_hed = case_when(
      str_detect(headline, "死刑", negate = TRUE) ~ 1),
    sixing_in_hed = replace_na(sixing_in_hed, 0),
    sixing_not_hed = replace_na(sixing_not_hed, 0)) %>%  
  select(headline, year, sixing_in_hed, sixing_not_hed)

dt_for_graph1 %>% 
  group_by(year) %>% 
  summarise(count_sixing_in_hed = n())

dt_for_graph1 %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(year) %>% 
  summarise(count_sixing_in_hed = sum(sixing_in_hed),
            count_sixing_not_hed = sum(sixing_not_hed)) %>% 
  ggplot(aes(year)) + 
  geom_line(aes(y = count_sixing_in_hed, colour = "death penalty in headline")) + 
  geom_line(aes(y = count_sixing_not_hed, colour = "death penalty not in headline"))




dt_for_graph1 %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(year) %>% 
  summarise(count_sixing_in_hed = sum(sixing_in_hed),
            count_sixing_not_hed = sum(sixing_not_hed)) %>% 
  ggplot(aes(year)) + 
  geom_line(aes(y = count_sixing_in_hed, colour = "death penalty in headline")) 



# DP in headline of article
dt_for_graph %>% 
  mutate(sixing_in_hed = case_when(
    str_detect(headline, "死刑") ~ 1),
    sixing_not_hed = case_when(
      str_detect(headline, "死刑", negate = TRUE) ~ 1),
    sixing_in_hed = replace_na(sixing_in_hed, 0),
    sixing_not_hed = replace_na(sixing_not_hed, 0)) %>%  
  mutate(date = ymd(date)) %>% 
  mutate(month = year(date)) %>% 
  group_by(year) %>% 
  # summarise(count_sixing_in_hed = sum(sixing_in_hed),
  # count_sixing_not_hed = sum(sixing_not_hed)) %>%
  # ungroup() %>%
  select(year, sixing_in_hed) %>% 
  filter(sixing_in_hed == 1) %>% 
  ggplot(aes(x = year)) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "'死刑' in headline in People's Daily by year") +
  geom_bar() 




# DP in body of article
dt_for_graph %>% 
  mutate(sixing_in_hed = case_when(
    str_detect(headline, "死刑") ~ 1),
    sixing_not_hed = case_when(
      str_detect(headline, "死刑", negate = TRUE) ~ 1),
    sixing_in_hed = replace_na(sixing_in_hed, 0),
    sixing_not_hed = replace_na(sixing_not_hed, 0)) %>%  
  mutate(date = ymd(date)) %>% 
  mutate(month = year(date)) %>% 
  group_by(year) %>% 
  select(year, sixing_not_hed) %>% 
  filter(sixing_not_hed == 1) %>% 
  ggplot(aes(x = year)) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "'死刑' in body of article in People's Daily by year") +
  geom_bar() 




# DP in EITHER body OR headline
dt_for_graph %>% 
  mutate(sixing_in_hed = case_when(
    str_detect(headline, "死刑") ~ 1),
    sixing_not_hed = case_when(
      str_detect(headline, "死刑", negate = TRUE) ~ 1),
    sixing_in_hed = replace_na(sixing_in_hed, 0),
    sixing_not_hed = replace_na(sixing_not_hed, 0)) %>%  
  mutate(date = ymd(date)) %>% 
  mutate(month = year(date)) %>% 
  group_by(year) %>% 
  ggplot(aes(x = year)) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "'死刑' in either the body of article or headline in People's Daily by year") +
  geom_bar() 








dt_for_graph1 %>% 
  mutate(year = as.numeric(year)) %>% 
  count(year) %>% 
  ggplot(aes(year)) + 
  geom_line(aes(y = n, colour = "all articles"))







########################3



dt_for_graph %>% 
  group_by(year) %>% 
  summarise(count = sum(sixing_in_hed))

dt_for_graph %>% 
  ggplot(aes(, as.numeric(count)), group = 1) + 
  geom_line()


head(chtoks_ngram[[1]], 30)


###################################################

library(jiebaR)
words <- c("　2015年，刑法修正案（九）决定在贪腐犯罪中引入终身监禁。今年4月，最高法、最高检又联合发布司法解释，明确了对重大贪污受贿罪犯可以决定终身监禁；终身监禁一 旦作出，就不受执行期间服刑表现的影响。这一制度的适用有效填补了死刑立即执行过重而死刑缓期执行过轻之间的落差，避免了贪官钻以往法律制度的空子，从刑事立法和司法的制度层面彻底“补漏”。同时，该制度保障了对贪腐行为施以“牢底坐穿”这一至为严厉的自由刑的法律威慑力，遵循了“少杀慎杀”的刑事司法政策，实现了严厉惩治腐败和严格限制死刑的双重理念的有机结合，是法治反腐的一大进步。")

engine1 <- worker(bylines = TRUE)

segment(words, engine1)

##########################

dp_dfm[year = 2010]


tstat_freq <- textstat_frequency(dp_dfm, groups = year)

dt_tstat  <-  as.data.table(tstat_freq)


dp_dfm <- dfm(chtoks_ngram)

tstat_freq1 <- textstat_frequency(dp_dfm, groups = year)

dt_tstat  <-  as.data.table(tstat_freq1)

dt_tstat[feature == "少杀"]



toks_test <- tokens(dp_corpus, remove_punct = TRUE) %>% 
  tokens_keep(pattern = "少杀慎杀")
dfmat_test <- dfm(toks_test)

tstat_freq <- textstat_frequency(dfmat_tweets, n = 5, groups = lang)
head(tstat_freq, 20)



tail(tstat_freq, 20)


dp_dfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()


tstat_key <- textstat_keyness(dp_dfm, 
                              target = dp_dfm$year >= 2015)
textplot_keyness(tstat_key)



###############################################333333














font_paths()

dp_dfm$page_num 

docvars(dp_dfm)

Sys.setlocale("LC_ALL", locale = "chs")


iris_cn <- iris
names(iris_cn) <- c("萼片长Sepal.Length","萼片宽Sepal.Width","花瓣长Petal.Length","花瓣宽Petal.Width","品种Species")
library(showtext)
ggplot(data = iris_cn, aes(x = 萼片长Sepal.Length)) + geom_histogram()


hist(iris_cn$萼片长Sepal.Length)

font_add_google("Schoolbell", "bell")

library(ggplot2)
theme < theme_get()
#using the Chinese fonts you have, check it with font book.  
theme$text$family <- "STFangsong"
theme_set(theme)

iris_cn <- iris
names(iris_cn) <- c("萼片长Sepal.Length","萼片宽Sepal.Width","花瓣长Petal.Length","花瓣宽Petal.Width","品种Species")
library(ggplot2)
ggplot(data = iris_cn, aes(x = 萼片长Sepal.Length)) + geom_histogram()

pdf("c:/cn.pdf", family = "GB1")
plot(1, main = "你好")  # use Chinese characters in the main title
dev.off()

library(showtext)
font_add("fang", "chinese-simfang.ttf") ## add font
pdf("showtext-ex1.pdf")




dp_dfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

# textplot_wordcloud(dp_dfm, max_words = 100, font = "SimHei")

dp_lsa <- textmodel_lsa(dp_dfm, nd = 10)


sources <-
  str_remove_all(rownames(dp_lsa$docs), "[0-9///'._txt]") 

sources.color <- rep("gray", times = length(sources))
sources.color[sources %in% "USA"] <- "blue"
sources.color[sources %in% "RUS"] <- "red"

plot(
  mylsa$docs[, 4:5],
  col = alpha(sources.color, 0.3),
  pch = 19,
  xlab = "Dimension 4",
  ylab = "Dimension 5",
  main = "LSA dimensions by subcorpus"
)


library(extrafont)

font_import()

fonts()

fonttable()

extrafont::font_import("SimHei")


textplot_wordcloud(ch_dfm, min_count = 500, random_order = FALSE,
                   rotation = .25, max_words = 100,
                   min_size = 0.5, max_size = 2.8,
                   font = "SimHei",
                   color = RColorBrewer::brewer.pal(8, "Dark2"))


ch_toks_orig <- dp_corpus %>% 
  tokens(remove_punct = TRUE)

# construct a dfm


head(rowSums(dp_dfm), 10)

topfeatures(dp_dfm)

dfmat_inaug_prop <- dfm_weight(dp_dfm, scheme  = "prop")
print(dfmat_inaug_prop)


kw_sx<- kwic(ch_toks_orig, pattern =  "死刑")

head(kw_sx, 10)



kwic(toks, pattern =  "死刑")











corp_tweets <- download(url = "https://www.dropbox.com/s/846skn1i5elbnd2/data_corpus_sampletweets.rds?dl=1")

toks_tweets <- tokens(corp_tweets, remove_punct = TRUE) %>% 
  tokens_keep(pattern = "#*")
dfmat_tweets <- dfm(toks_tweets)

tstat_freq <- textstat_frequency(dfmat_tweets, n = 5, groups = lang)
head(tstat_freq, 20)






ch17_corp <- corpus_subset(dp_corpus, journal_year == "2015")
ch17_toks <- 
  tokens(ch17_corp, remove_punct = TRUE) %>% 
  tokens_remove(ch_stop)
ch_fcm <- fcm(ch17_toks, context = "window")  
topfeatures(ch_fcm["法制", ])

# corp <- corpus_reshape(dp_corpus, to = "paragraphs")
toks <- tokens(my_corpus1, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(pattern = stopwords("zh_cn", source = "marimo"), min_nchar = 2) %>% 
  tokens_keep(pattern = "^\\p{script=Hani}+$", valuetype = 'regex')

print(toks[2], max_ndoc = 1, max_ntok = -1)


second_dfm <- dfm(toks)

tstat_freq <- textstat_frequency(second_dfm, n = 100)

second_dfm





df_files_metadata <- fread("/mnt/c/Users/m/projects/peoples_daily_database/data/df_files_metadata.csv")

head(docvars(my_corpus1))

docvars(my_corpus1, field = "date")


kwic(toks, pattern =  "死刑")


tstat_col_caps <- tokens_select(toks, pattern = "^\\p{script=Hani}+$", 
                                valuetype = "regex", 
                                case_insensitive = FALSE, 
                                padding = TRUE) %>% 
  textstat_collocations(min_count = 100)


tstat_freq <- textstat_frequency(dfmat_tweets, n = 5, groups = lang)
head(tstat_freq, 20)

head(tstat_col_caps, 20)







































dt

library(textnets)

library(quanteda)




View(data_char_ukimmig2010)

my_corpus <- corpus(data_char_ukimmig2010)  # build a new corpus from the texts
summary(my_corpus)

dt_corpus <- dt[,-c("V")]

names(dt)
dt_clean <- dt[,.(document_id, abstract_ch, journal_year)]

dp_corpus <- corpus(dt_clean, docid_field = "document_id", text_field = "abstract_ch", meta = "journal_year")



options(datatable.prettyprint.char=20L)


dpcorpsum <- summary(dp_corpus)


dt_tstat_freq <- as.data.table(tstat_freq)

dt_tstat_freq[feature == "少杀"]










head(dpcorpsum, n =10)

ch_stop <- stopwords("zh", source = "misc")

# tokenize
ch_toks <- dp_corpus %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = ch_stop)

# construct a dfm
ch_dfm <- dfm(ch_toks)
topfeatures(ch_dfm)



set.seed(100)

ch17_corp <- corpus_subset(dp_corpus, journal_year == "2015")
ch17_toks <- 
  tokens(ch17_corp, remove_punct = TRUE) %>% 
  tokens_remove(ch_stop)
ch_fcm <- fcm(ch17_toks, context = "window")  
topfeatures(ch_fcm["法制", ])

# corp <- corpus_reshape(dp_corpus, to = "paragraphs")
toks <- tokens(dp_corpus, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(pattern = stopwords("zh_cn", source = "marimo"), min_nchar = 2) %>% 
  tokens_keep(pattern = "^\\p{script=Hani}+$", valuetype = 'regex')
print(toks[2], max_ndoc = 1, max_ntok = -1)

tstat_col_caps <- tokens_select(toks, pattern = "^\\p{script=Hani}+$", 
                                valuetype = "regex", 
                                case_insensitive = FALSE, 
                                padding = TRUE) %>% 
  textstat_collocations(min_count = 100)
head(tstat_col_caps, 20)

tmod_lda <- textmodel_lda(dfmat, k = 5)

terms(tmod_lda, 10)

head(topics(tmod_lda), 20)











tstat_freq <- textstat_frequency(dfmat)
head(tstat_freq, 20)

dfmat %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

tstat_col2 <- tokens_select(ch_toks, pattern = "^\\p{script=Hani}+$", 
                            valuetype = "regex", 
                            padding = TRUE) %>% 
  textstat_collocations(min_count = 20, size = 2)

dt_tstat_col2 <- as.data.table(tstat_col2)

dt_tstat_col2[collocation %like% "少杀"]










head(tstat_col2, 20)
dfmattest <- 
  tstat_freq <- textstat_frequency(dfmattest, n = 5)
head(tstat_freq, 20)


dfmat







dt[, .N, by = journal_year] %>% 
  ggplot(aes(journal_year)) +
  geom_histogram(binwidth = 1)



dt [journal_year > 1975] %>% 
  ggplot(aes(journal_year)) +
  geom_histogram(binwidth = 1)


tokenInfo <- summary(dp_corpus)


if (require(ggplot2))
  ggplot(data=tokenInfo, aes(x = journal_year, y = Tokens, group = 1)) + geom_line() + geom_point() +
  scale_x_continuous(labels = c(seq(1980, 2020, 12)), breaks = seq(1980, 2020, 12)) +
  theme_bw()











`%notlike%` <- Negate(`%like%`)
dt[title_ch %notlike% "死刑"][order(-downloads), .(title_ch, downloads, journal_year)] %>% View()

dt[abstract_ch %like% "移植" & journal_year >　1900, .(journal_year)] %>% 
  ggplot(aes(journal_year)) +
  geom_histogram(binwidth = 1)

dt[abstract_ch %like% "移植"]











