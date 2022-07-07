# ST558_Project2

## Purpose of this repo

In this repo, we are going to analyse an online news popularity data set from [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity). This dataset summarizes a heterogeneous set of features about articles published by Mashable in a period of two years. 

Our goal is to predict the number of shares in social networks (popularity) within each channel of articles. We are going to summarize and build predictive model on the data subsetted by data_channel_is_*. We are going to automatically render each document from a single .Rmd file with `params`. 

## Required packages

* readr: read in data as tibble.

* dplyr: manipulating data.

* ggplot2: visualizing the data.

* corrplot: plotting correlation between variables.

* caret: preprocessing data, training model, doing cross validation and evaluating model on test sets. 

## Links to analysis for each type of article:

[Bus articles ia available here](/blob/main/documents/bus.html)

[Entertainment articles ia available here](documents/entertainment.html)

[Lifestyle articles ia available here](documents/lifestyle.html)

[Socmed articles ia available here](documents/socmed.html)

[Tech articles ia available here](documents/tech.html)

[World articles ia available here](documents/world.html)

## render code

* set up list of file name and parameters

library(tidyverse)
filter_type <- c("entertainment", "bus", "tech", "lifestyle", "world", "socmed")
output_file=paste0(filter_type, ".md")
params <- lapply(filter_type, FUN=function(x){list(filter_type=x)})
reports <- tibble(output_file, params)

* code to create reports automatically 


apply(reports, MARGIN=1, FUN=function(x){
rmarkdown::render(input="Code/Project2.Rmd", output_file=x[[1]], output_format="github_document", output_dir="documents/", params=x[[2]], output_options=list(html_preview=FALSE, toc=TRUE, toc_depth=2))
})