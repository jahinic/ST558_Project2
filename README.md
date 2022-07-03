# ST558_Project2

## render code

* set up list of file name and parameters

library(tidyverse)
filter_type <- c("entertainment", "bus", "tech", "lifestyle", "world", "socmed")
output_file=paste0(filter_type, ".md")
params <- lapply(filter_type, FUN=function(x){list(filter_type=x)})
reports <- tibble(output_file, params)

* code to create reports automatically 

library(rmarkdown)
apply(reports, MARGIN=1, FUN=function(x){
render(input="Code/Project2.Rmd", output_file=x[[1]], output_format="github_document", output_dir="documents/", params=x[[2]], output_options=list(html_preview=FALSE))
})