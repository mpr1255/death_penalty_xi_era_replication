library(data.table)
library(rvest) 
library(httr)
library(tidyverse)
library(RSelenium)
library(wdman)
library(curl)
library(jsonlite)
library(glue)


###################################

portal  <-  "nla"
portal_flag <- glue("/mnt/c/Users/m/projects/death_penalty_xi_era/ref_files/{portal}_flag")

if (!file.exists(portal_flag)){
  writeLines(portal, portal_flag)  
}else{
  portal <- "anu"
}



curl <- read_lines(glue("/mnt/c/Users/m/projects/death_penalty_xi_era/ref_files/curl_{portal}.txt")) %>% 
  as_tibble() %>%  
  rename(raw = value) %>% 
  mutate(key = str_match(raw, "(')(.*?)(\\:)")[,3]) %>% 
  mutate(value = str_match(raw, "(\\:)( .*?)(\\')")[,3]) %>% 
  mutate(value = str_trim(value))

h <- new_handle()
handle_setheaders(h,
                  "Connection" = curl[["value"]][2],
                  "Upgrade-Insecure-Requests" = curl[["value"]][3],
                  "User-Agent" = curl[["value"]][4],
                  "Accept" = curl[["value"]][5],
                  "Sec-GPC" = curl[["value"]][6],
                  "Referer" = curl[["value"]][7],
                  "Accept-Language" = curl[["value"]][8],
                  "Cookie" = curl[["value"]][9])

url <- str_match(curl[1,1][[1]], "(http.*?)(\\')")[,2]
url_pieces <- url %>% str_split("record=1", n = 3)


# file_list <- list.files("/mnt/c/Users/m/projects/death_penalty_xi_era/data/peoples_daily", pattern = "html") %>% 
#   as_tibble() 
# 
# max_number <- file_list %>% 
#   mutate(number = parse_number(str_match(value, "(pd_death_penalty-)([0-9]{1,4})")[,3])) %>% 
#   summarise(max_number = max(number))
# 
# if (!is.na(max_number)){
#   i_urls_start <- max_number[[1]]
# }else{
#   i_urls_start <- 1
# }

i_urls_end <- 5554L
i_urls_start <- 1

url_list <- list()


path <- "/mnt/c/Users/m/projects/death_penalty_xi_era/data/peoples_daily/"


for (i in i_urls_end:i_urls_start){
  
  if (file.exists(glue("{path}pd_url_{i}"))){
    print(glue("pd_url_{i} already exists; going to the next one"))
    next
  }
  
  empty_string <- "NULL"
  
  writeLines(empty_string, glue("{path}pd_url_{i}"))
  
  url_list[[i]] <- glue(url_pieces[[1]][1], "record={i}", url_pieces[[1]][2])
  
  tmp <- tempfile()
  
  curl_download(url_list[[i]], tmp, handle = h)
  
  old_filename <- glue("{path}pd_url_{i}.html")
  
  writeLines(readLines(tmp), old_filename)
  
  readtemp <- read_html(old_filename, encoding = "GB18030")
  
  headline <- readtemp %>% 
    html_node(".biaoti-18b") %>% 
    html_text()
  
  date <- readtemp %>% 
    html_nodes(".hui-12") %>% 
    html_text() %>% 
    str_extract(., "[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}第[0-9]{0,4}版")
  
  new_filename <- glue("{path}pd_death_penalty-{i}-{date}-{headline}.html")
  
  if (file.exists(new_filename)){
    print(glue("pd_death_penalty-{i}-{date}-{headline}.html already exists; going to the next file"))
    unlink(old_filename)
  }else{
    file.rename(old_filename, new_filename)
    print(glue("Saving {new_filename}...."))
  }
  Sys.sleep(10) 
    
}
unlink(portal_flag)

