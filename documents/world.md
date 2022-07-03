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

-   weekday: day on which the article published

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

# create predictor weekday 
data <- target_data %>% mutate(weekday=ifelse(weekday_is_monday==1, "Monday", ifelse(weekday_is_tuesday==1, "Tuesday", ifelse(weekday_is_wednesday==1, "Wednesday", ifelse(weekday_is_thursday==1, "Thursday", ifelse(weekday_is_friday==1, "Friday", ifelse(weekday_is_saturday==1, "Saturday", ifelse(weekday_is_sunday==1, "Sunday", NA))))))))

data <- data %>% select(shares, timedelta, num_self_hrefs, num_imgs, num_videos,  num_keywords, weekday, is_weekend, LDA_00, LDA_01, LDA_02, LDA_03, LDA_04, global_rate_negative_words, global_rate_positive_words)

data
```

    ## # A tibble: 7,057 x 15
    ##    shares timedelta num_self_hrefs
    ##     <dbl>     <dbl>          <dbl>
    ##  1    593       731              2
    ##  2   1200       731              0
    ##  3   2100       731              4
    ##  4   1200       731              4
    ##  5   4600       731              3
    ##  6   1200       731              3
    ##  7    631       731              3
    ##  8   1300       730              4
    ##  9   1700       730              2
    ## 10    455       729              1
    ## # ... with 7,047 more rows, and 12 more
    ## #   variables: num_imgs <dbl>,
    ## #   num_videos <dbl>,
    ## #   num_keywords <dbl>, weekday <chr>,
    ## #   is_weekend <dbl>, LDA_00 <dbl>,
    ## #   LDA_01 <dbl>, LDA_02 <dbl>,
    ## #   LDA_03 <dbl>, LDA_04 <dbl>, ...

-   Split data into train and test sets

``` r
library(caret)
set.seed(100)
train_index <- createDataPartition(data$weekday, p=0.7, list=FALSE)
train <- data[train_index,]
test <- data[-train_index, ]
train
```

    ## # A tibble: 4,943 x 15
    ##    shares timedelta num_self_hrefs
    ##     <dbl>     <dbl>          <dbl>
    ##  1    593       731              2
    ##  2   2100       731              4
    ##  3   1200       731              4
    ##  4   4600       731              3
    ##  5   1200       731              3
    ##  6    631       731              3
    ##  7   1300       730              4
    ##  8   1700       730              2
    ##  9    455       729              1
    ## 10   1900       729              2
    ## # ... with 4,933 more rows, and 12 more
    ## #   variables: num_imgs <dbl>,
    ## #   num_videos <dbl>,
    ## #   num_keywords <dbl>, weekday <chr>,
    ## #   is_weekend <dbl>, LDA_00 <dbl>,
    ## #   LDA_01 <dbl>, LDA_02 <dbl>,
    ## #   LDA_03 <dbl>, LDA_04 <dbl>, ...

## Summarizations on train set

-   descriptive statistics on response variable

``` r
summary_response <- summary(train$shares)
summary_response 
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu. 
    ##      47     830    1200    2889    2100 
    ##    Max. 
    ##  193400

``` r
sd_response <- sd(train$shares)
sd_response
```

    ## [1] 6770.211

The minimum value of shares is 47, maximum value is 1.934^{5}, mean is
2889.3267247, median is 1200, and standard deviation is 6770.2113135.

-   summarization across `weekday` predictor

``` r
train %>% group_by(weekday) %>% summarize(n=n(), min=min(shares), max=max(shares), avg=mean(shares), median=median(shares))
```

    ## # A tibble: 7 x 6
    ##   weekday     n   min    max   avg median
    ##   <chr>   <int> <dbl>  <dbl> <dbl>  <dbl>
    ## 1 Friday    681    58  82200 2845.   1200
    ## 2 Monday    951    59 112600 2841.   1100
    ## 3 Saturd~   266    65  35100 3167.   1600
    ## 4 Sunday    376   536  69500 3791.   1700
    ## 5 Thursd~   862    57 193400 2828.   1100
    ## 6 Tuesday   900    47  87600 2739.   1100
    ## 7 Wednes~   907    51  98500 2726.   1100

``` r
g <- ggplot(train %>% filter(shares<quantile(shares, p=0.75)), aes(x=shares))
g + geom_histogram(aes(fill=weekday), position="stack")
```

![](C:/NCSU/Git/ST558_Project2/images/unnamed-chunk-6-1.png)<!-- -->

``` r
g <- ggplot(train %>% filter(shares<quantile(shares, p=0.75)), aes(x=shares))
g + geom_histogram(aes(fill=is_weekend), position="stack", bins=30)
```

![](C:/NCSU/Git/ST558_Project2/images/unnamed-chunk-7-1.png)<!-- -->
