
dt <-  fread(r"(C:\Users\m\projects\cnki_database\data\run_5\tbls\tbl_articles_run_5_after_stage_4.csv)", encoding = "UTF-8")

dt[,pdf_url := str_remove(pdf_url, "http://")]

dt[,pdf_url := str_replace(pdf_url, ".rp.nla.gov.au/kcms", "/kns55")]

dt[pdf_url %like% ".rp.nla.gov.au/kcms", .(pdf_url)]



options(datatable.prettyprint.char=10L)
Sys.setlocale("LC_CTYPE", locale = "chs")
```

Simple exploratory stuff, just trying to get a sense of where the leaders are popping up
```{r}
dt[title_ch %like% "死刑" & abstract_ch %like% "习近平"][,abstract_ch]
dt[title_ch %like% "死刑" & abstract_ch %like% "胡锦涛"]
dt[title_ch %like% "死刑" & abstract_ch %like% "江泽民"]

dt[abstract_ch %like% "习近平" & abstract_ch %like% "死刑"][,abstract_ch]
dt[abstract_ch %like% "胡锦涛" & abstract_ch %like% "死刑"][,abstract_ch]
dt[abstract_ch %like% "江泽民" & abstract_ch %like% "死刑"][,abstract_ch]


