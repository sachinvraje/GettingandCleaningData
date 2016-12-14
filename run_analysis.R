#run_analysis.r

require(knitr)
require(markdown)
setwd("/Users/Sachin/GettingandCleaningData/GettingandCleaningData")
knit("run_analysis.Rmd", encoding="ISO8859-1")
markdownToHTML("run_analysis.md", "run_analysis.html")
