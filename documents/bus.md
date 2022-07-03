ST 558 Project 2
================
John Hinic & Fang Wu
2022-07-01

## Introduction

We are going to analyze an online news popularity data set from [Machine
Learning
Repository](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity#).
This data set summarizes a heterogeneous set of features about articles
published by Mashable in a period of two years. The goal is to predict
the number of shares in social networks (popularity).

We are going to analyze different type of article separately and use
following predictors:

-   timedelta: Days between the article publication and the data set
    acquisition

-   num_self_hrefs: Number of links to other articles published by
    Mashable

-   num_imgs: Number of images

-   num_videos: Number of videos

-   num_keywords: Number of keywords in the metadata

-   is_weekend: Was the article published on the weekend?

-   LDA_00: Closeness to LDA topic 0

-   LDA_01: Closeness to LDA topic 1

-   LDA_02: Closeness to LDA topic 2

-   LDA_03: Closeness to LDA topic 3

-   LDA_04: Closeness to LDA topic 4

-   global_rate_positive_words: Rate of positive words in the content

-   global_rate_negative_words: Rate of negative words in the content

In order to predict the number of shares, we are going to use

## Prepare Data

We’ll use the `readr` and `dplyr` packags from the `tifyverse`. First,
we are going to read in data as tibble using function `read_csv`. Next,
in order to access different data channel of interest automatically, we
are going to create a variable called `type`. Last we `filter` the data
channel of interest using `params$type` and `select` predictors we are
going to work on.

``` r
# read in raw data
raw_data <- read_csv("C:/NCSU/Git/ST558_Project2/Data/OnlineNewsPopularity.csv") 

# create type column for different daa channel

type_data <- raw_data %>% mutate(type=ifelse(data_channel_is_lifestyle==1, "lifestyle", ifelse(data_channel_is_entertainment==1, "entertainment", ifelse(data_channel_is_bus==1, "bus", ifelse(data_channel_is_socmed==1, "socmed", ifelse(data_channel_is_tech==1, "tech", ifelse(data_channel_is_world==1, "world", NA)))))))
```

``` r
# select data for one data channel of interest with predictors for analyzing
library(dplyr)
target_data <- type_data %>% filter(type == params$filter_type) 
target_data
```

    ## # A tibble: 6,258 x 62
    ##    url           timedelta n_tokens_title
    ##    <chr>             <dbl>          <dbl>
    ##  1 http://masha~       731              9
    ##  2 http://masha~       731              9
    ##  3 http://masha~       731              8
    ##  4 http://masha~       731             13
    ##  5 http://masha~       731             11
    ##  6 http://masha~       731              8
    ##  7 http://masha~       731             10
    ##  8 http://masha~       731             12
    ##  9 http://masha~       731              6
    ## 10 http://masha~       730             13
    ## # ... with 6,248 more rows, and 59 more
    ## #   variables: n_tokens_content <dbl>,
    ## #   n_unique_tokens <dbl>,
    ## #   n_non_stop_words <dbl>,
    ## #   n_non_stop_unique_tokens <dbl>,
    ## #   num_hrefs <dbl>,
    ## #   num_self_hrefs <dbl>, ...

``` r
#%>% select(timedelta, num_self_hrefs, num_imgs, num_videos,  num_keywords, is_weekend, LDA, global_rate_negative_words, global_rate_positive_words)
```

## 
