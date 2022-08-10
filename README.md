**Replication files for Smith, Tobias, Sue Trevaskes and Matthew Robertson, "(Not) talking about the death penalty in the Xi Jinping era" forthcoming in the International Journal for Crime, Justice and Social Democracy**

=====================================

This repository hosts the replication materials and code for the above paper.

Four sources of data were used for this project. An explanation of each of the data sources, how they were gathered and preprocessed, appears below. 

Several of the original data sources are not publicly available, primarily due to copyright concerns. This is noted in the relevant section. The graphics used from the metadata from those sources, however, are in this repo, and they can be reproduced based on the code provided. 

Direct any questions about the data to m dot p dot robertson at anu dot edu dot au. It is hoped, however, that any interested party can simply clone or browse this repo and have a thorough understanding of the data used in the paper, how it was processed, and be able to reproduce the analysis.

## People's Daily corpus (only partially available)

**About the data source:** This project uses a subset of the full People's Daily dataset administered by Matthew Robertson. A presentation based on this project is here: https://github.com/mpr1255/quickshare/blob/master/polmeth_pres.pdf 

How that dataset was filtered for this paper is visible in `code/peoples_daily_analysis.R`, which also contains the code for the graphs.

The dataset is updated daily, and we use data up until 01/15/2022 for this project (there were no death penalty-related articles in 2022 so effectively the relevant data ends in 2021.)

## CNKI publications (metadata available only)

**About the data source:** CNKI data was obtained using the website's own summary functions. These are visible by conducting a search, then going to `Export and analysis > Visual analysis > Analysis of all search results`. The URL is then of the form `https://oversea.cnki.net/kns/Visual/Center`. We obtained the numbers from the graphs from the underlying JSON there.

## PRC leader collected works and Xi Jinping speeches (fully available)

**About the data source:** These materials did not come from a single repository like the other two sources. They were obtained by searches using Google and Baidu, Library Genesis, and official websites. Xi's speeches were scraped with `R` and cURL. 