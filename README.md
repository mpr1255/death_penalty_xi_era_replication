**Replication files for Smith, Tobias, Sue Trevaskes and Matthew Robertson, "(Not) talking about the death penalty in the Xi Jinping era" forthcoming in the International Journal for Crime, Justice and Social Democracy**

=====================================

This repository hosts the replication materials and code for the above paper.

Four sources of data were used for this project. An explanation of each of the data sources, how they were gathered and preprocessed, appears below. 

Several of the original data sources are not publicly available, primarily due to copyright concerns. This is noted in the relevant section. The graphics used from the metadata from those sources are however in this repo, and they can be reproduced based on the code provided.

## People's Daily corpus (only partially available)

**About the data source:** This project uses a subset of the full People's Daily dataset administered by Matthew Robertson. A presentation based on this project is here: https://github.com/mpr1255/quickshare/blob/master/polmeth_pres.pdf 

How that dataset was filtered for this paper is visible in `code/peoples_daily_analysis.R`, which also contains the code for the graphs.

The dataset is updated daily, and we use data up until 01/15/2022 for this project (there were no death penalty-related articles in 2022 so effectively the relevant data ends in 2021.)

## CNKI publications (metadata available only)

**About the data source:** CNKI data was 

### About the OCR process
Nearly all the pdfs were converted with pdftotext on the command line using the -raw flag. 

There were 1109 files that could not be converted (measured as any file less than 1kb). Those >1kb files were moved out of the txt folder and into old_text and the files in ocr_text moved in. ocr_text also got zipped to keep it in the exact, pristine state it existed in after the OCR. 

OCR was done with tesseract 4.0 through R, first in R studio then Rscript (hence the absolute paths). The scripts for that are `dp_ocr.R` (and `dp_ocr-reverse.R`; I'll figure out a way to do it in the same file next time...)

All of those text files are in ocr_raw. 

ocr_clean are files that have whitespace deleted. Those are used for the analysis.

Whitespace was deleted with this command: 
`for f in *.txt; do tr -d " \t\n\r" < "$f" > "/path/to/dest/${f%.txt}"--clean.txt; done`





## PRC leader collected works and Xi Jinping speeches (fully available)






The data used for this project was pulled from a large, local database. 

The files are 


