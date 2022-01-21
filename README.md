

# Project workflow
The data used for this project was pulled from a large, local database. 

The files are 


# About the OCR process
Nearly all the pdfs were converted with pdftotext on the command line using the -raw flag. 

There were 1109 files that could not be converted (measured as any file less than 1kb). Those >1kb files were moved out of the txt folder and into old_text and the files in ocr_text moved in. ocr_text also got zipped to keep it in the exact, pristine state it existed in after the OCR. 

OCR was done with tesseract 4.0 through R, first in R studio then Rscript (hence the absolute paths). The scripts for that are `dp_ocr.R` (and `dp_ocr-reverse.R`; I'll figure out a way to do it in the same file next time...)

All of those text files are in ocr_raw. 

ocr_clean are files that have whitespace deleted. Those are used for the analysis.

Whitespace was deleted with this command: 
`for f in *.txt; do tr -d " \t\n\r" < "$f" > "/path/to/dest/${f%.txt}"--clean.txt; done`

