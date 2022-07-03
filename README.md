# ST558_Project2

## render code

* set up list of file name and parameters

type <- unique(type_data$type)
output_file=paste0(type, ".md")
params <- lapply(type, FUN=function(x){list(type=x)})
reports <- tibble(output_file, params)

* code to create reports automatically 

library(rmarkdown)
apply(reports, MARGIN=1, FUN=function(x){
render(input="code/Project2.Rmd", output_file=x[[1]], output_format="github_document", output_dir="documents/", params=x[[2]])
})