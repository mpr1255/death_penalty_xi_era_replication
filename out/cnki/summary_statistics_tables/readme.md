	.
	├── CNKI tables.docx
	├── readme.md
	├── sixing_sihuan_cites_downloads_pianming_guanjianci_zhaiyao.xlsx
	├── sixing_sihuan_trend.png
	├── xingshi_sifa_trend.png
	├── sixing_sihuan_guanjianci_counts.json
	└── xingshi_sifa_guanjianci_counts.json

The names of the files indicate the content and how they were obtained. 

guanjianci means the search was in the 关键词 (keyword) field in CNKI. 
zhaiyao = 摘要 (abstract)
pianming = 篇名 (title)


An explanation of the files follow.

	├── CNKI tables.docx

This is the copy-pasted result from the html file in /code/make_cnki_tables.html, which was produced from the R script with that name. These just grab the data from the following excel file and turn it into html tables and then these are copy-pasted into Word.

	├── sixing_sihuan_cites_downloads_pianming_guanjianci_zhaiyao.xlsx

The data in this Excel file was manually copy-pasted from the CNKI website.

Note that the file contains searches that are different to the the two json files.

Respectively those searches yielded:
3,380 results from 1/2003-1/2013
1,745 results from 1/2013-1/2022

The English-language columns are unedited output from Google Translate for guidance purposes only. Consult the original Chinese for the proper citation.

	├── sixing_sihuan_trend.png
	├── xingshi_sifa_trend.png

These are screenshots from the source explained below. The json files are the raw data from these graphs. See underneath.


	├── sixing_sihuan_guanjianci_counts.json
	└── xingshi_sifa_guanjianci_counts.json

The json files were obtained by pulling data directly off the graph that is displayed after clicking the following buttons in the new CNKI interface, after having conducted a search:
导出与分析 > 可视化分析 > 全部检索结果分析

This yields the following URL: https://chn-oversea-cnki-net.virtual.anu.edu.au/kns/Visual/Center which displays the graphs (above.)