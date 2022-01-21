explanation.txt is a draft of a few paragraphs about our analysis of death penalty across the published writings of all the leaders, as well as the online speeches of XJP. The text is based on the data in output.html, which was created from analysing the text of the speeches (in the data folder, not shared because it's many hundreds of megabytes.)

The output file was created by running the following unix command in the entire data/leader_collected_works/sorted folder, which includes the raw and OCRed speeches of all of the leaders, as well as the final volume of the collected works of Xi Jinping: 

rga -t pdf -t txt --color=always --heading --colors 'match:fg:red' --colors 'match:bg:blue' --colors 'line:bg:yellow' -C 10 --context-separator @@@@@@@@@@@@@ "死刑|死\s+刑|死缓|死\s+缓|处死|处\s+死|枪毙|枪\s+毙|处决|处\s+决" | aha > output.html

this command does the following:
invokes ripgrep to do a regular expression search through all pdf and txt files (the -t), colors them for ease of reference, provides 10 lines before and after (-C 10), puts in some context separators (@@@), and searches for a range of death penalty related terms and permutations using regular expressions (to account for poor OCR etc.), pipes this result to the 'aha' package, and outputs a html file that is easy to read.

The output.html file was then analyzed qualitatively. 