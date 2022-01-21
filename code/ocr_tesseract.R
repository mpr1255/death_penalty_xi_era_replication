library(data.table)
library(glue)
library(tesseract)
library(here)
library(purrr)
library(stringr)

here <- rprojroot::find_rstudio_root_file()

files_to_ocr <- list.files(glue("{here}/data/leader_collected_works/sorted/raw/"), full.names = T)

custom_settings <- tesseract(language = "chi_sim", options = list(tessedit_pageseg_mode = 1))

ocr_save_text <- function(file){
  file_out <- str_replace(file, "\\.pdf", "\\.txt")
  if (file.exists(file_out)){
    print("file exists; not converted")
  }else{
    fulltext <- ocr(file, engine = custom_settings)
    writeLines(fulltext, file_out, useBytes = TRUE)
  }
}

map(files_to_ocr, ~ocr_save_text(.x))

