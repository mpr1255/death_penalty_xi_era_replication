#!/usr/bin/env Rscript

library(data.table)
library(tidyverse)
library(glue)
library(tesseract)

##################

txt_for_ocr <- list.files("/mnt/c/Users/m/projects/death_penalty_xi_era/data/txt/", full.names = TRUE) %>% as_tibble()

txt_for_ocr <- list.files("/mnt/c/Users/m/projects/death_penalty_xi_era/data/txt/", full.names = TRUE) %>% as_tibble()

txt_for_ocr <- txt_for_ocr %>% 
  mutate(file_size = as.integer(file.size(txt_for_ocr$value))) %>% 
  filter(file_size < 2000) %>% 
  mutate(doc_id = str_extract(value, "(C.*?--)")) %>% 
  mutate(doc_id = str_remove(doc_id, "--"))

pdf_list <- list.files("/mnt/c/Users/m/projects/death_penalty_xi_era/data/pdf/", full.names = TRUE) %>% as_tibble() %>% 
  mutate(doc_id = str_extract(value, "(C.*?--)")) %>% 
  mutate(doc_id = str_remove(doc_id, "--"))

pdfs_to_ocr <- pdf_list %>% filter(doc_id %in% txt_for_ocr$doc_id)

pdfs_to_ocr <- pdfs_to_ocr %>% arrange(desc(value))

custom_settings <- tesseract(language = "chi_sim", options = list(tessedit_pageseg_mode = 1))

ocr_save_text <- function(file){
  file_name <- str_remove(str_extract(file, "C.*?\\.pdf"), ".pdf")
  if (file.exists(glue("/mnt/c/Users/m/projects/death_penalty_xi_era/data/ocr_txt/{file_name}.txt"))){
    print("file exists; not converted")
  }else{
    fulltext <- ocr(file, engine = custom_settings)
    writeLines(fulltext, glue("/mnt/c/Users/m/projects/death_penalty_xi_era/data/ocr_txt/{file_name}.txt"), useBytes = TRUE)
  }
}

map(pdfs_to_ocr$value, ~ocr_save_text(.x))
