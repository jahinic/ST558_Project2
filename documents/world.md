ST 558 Project 2
================
John Hinic & Fang Wu
2022-07-01

-   [Introduction](#introduction)
-   [Prepare Data](#prepare-data)
-   [Summarizations on train set](#summarizations-on-train-set)

## Introduction

The consumption of online news is expediting day by day due to the
extensive adoption of smartphones and the rise of social networks.
Online news can capture the eye of a signiﬁcant amount of Internet users
within a brief period of your time. Prediction of online news popularity
helps news organizations to gain better insights into the audience
interests and to deliver more relevant and appealing content in a
proactive manner. The company can allocate resources more wisely to
prepare stories over their life cycle. Moreover, prediction of news
popularity is also beneﬁcial for trend forecasting, understanding the
collective human behavior, advertisers to propose more proﬁtable
monetization techniques,and readers to ﬁlter the huge amount of
information quickly and efﬁciently.

We are going to analyze and predict the number of shares within
different data channel of interest using an online news data set from
[Machine Learning
Repository](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity#)
. This data set summarizes a heterogeneous set of features about
articles published by Mashable in a period of two years.

-   We are going to focuse on the following predictors:

    1.  url: URL of the article (non-predictive)

    2.  timedelta: Days between the article publication and the dataset
        acquisition (non-predictive)

    3.  n_tokens_title: Number of words in the title

    4.  n_tokens_content Number of words in the content

    5.  n_unique_tokens: Rate of unique words in the content

    6.  n_non_stop_unique_tokens: Rate of unique non-stop words in the
        content

    7.  num_hrefs: Number of links

    8.  num_self_hrefs: Number of links to other articles published by
        Mashable

    9.  num_imgs: Number of images

    10. num_videos: Number of videos

    11. average_token_length: Average length of the words in the content

    12. num_keywords: Number of keywords in the metadata

    13. self_reference_min_shares: Min. shares of referenced articles in
        Mashable

    14. self_reference_max_shares: Max. shares of referenced articles in
        Mashable

    15. self_reference_avg_sharess: Avg. shares of referenced articles
        in Mashable

    16. global_subjectivity: Text subjectivity

    17. global_sentiment_polarity: Text sentiment polarity

    18. global_rate_positive_words: Rate of positive words in the
        content

    19. global_rate_negative_words: Rate of negative words in the
        content

    20. rate_positive_words: Rate of positive words among non-neutral
        tokens

    21. rate_negative_words: Rate of negative words among non-neutral
        tokens

    22. title_subjectivity: Title subjectivity

    23. title_sentiment_polarity: Title polarity

    24. abs_title_subjectivity: Absolute subjectivity level

    25. abs_title_sentiment_polarity: Absolute polarity level

    26. shares: Number of shares (target)

Stop Words usually refer to the most common words in a language, there
is no single universal list of stop words used by all natural language
processing tools. For some search engines, these are some of the most
common, short function words, such as the, is, at, which, and on.

## Prepare Data

We’ll use the `readr` and `dplyr` packages from `tifyverse`. First, we
are going to read in data as tibble using function `read_csv`. Next, in
order to access different data channel of interest automatically, we are
going to create a variable called `type`. Last we `filter` the data
channel of interest using `params$` automatically.

-   Read in raw data and create new variable `type`

``` r
# read in raw data
raw_data <- read_csv("C:/NCSU/Git/ST558_Project2/Data/OnlineNewsPopularity.csv") 

# create type column for different data channel
type_data <- raw_data %>% mutate(type=ifelse(data_channel_is_lifestyle==1, "lifestyle", ifelse(data_channel_is_entertainment==1, "entertainment", ifelse(data_channel_is_bus==1, "bus", ifelse(data_channel_is_socmed==1, "socmed", ifelse(data_channel_is_tech==1, "tech", ifelse(data_channel_is_world==1, "world", NA)))))))
```

-   Subset data channel of interest automatically with `params`

``` r
# select data for data channel of interest
library(dplyr)
target_data <- type_data %>% filter(type == params$filter_type) 
target_data
```

    ## # A tibble: 8,427 x 62
    ##    url                timedelta
    ##    <chr>                  <dbl>
    ##  1 http://mashable.c~       731
    ##  2 http://mashable.c~       731
    ##  3 http://mashable.c~       731
    ##  4 http://mashable.c~       731
    ##  5 http://mashable.c~       731
    ##  6 http://mashable.c~       731
    ##  7 http://mashable.c~       731
    ##  8 http://mashable.c~       731
    ##  9 http://mashable.c~       731
    ## 10 http://mashable.c~       730
    ## # ... with 8,417 more rows,
    ## #   and 60 more variables:
    ## #   n_tokens_title <dbl>,
    ## #   n_tokens_content <dbl>,
    ## #   n_unique_tokens <dbl>,
    ## #   n_non_stop_words <dbl>,
    ## #   n_non_stop_unique_tokens <dbl>, ...

-   Split data into train and test sets

``` r
library(caret)
set.seed(100)
train_index <- createDataPartition(target_data$is_weekend, p=0.7, list=FALSE)
train <- target_data[train_index,]
test <- target_data[-train_index, ]
train
```

    ## # A tibble: 5,899 x 62
    ##    url                timedelta
    ##    <chr>                  <dbl>
    ##  1 http://mashable.c~       731
    ##  2 http://mashable.c~       731
    ##  3 http://mashable.c~       731
    ##  4 http://mashable.c~       731
    ##  5 http://mashable.c~       731
    ##  6 http://mashable.c~       731
    ##  7 http://mashable.c~       731
    ##  8 http://mashable.c~       730
    ##  9 http://mashable.c~       729
    ## 10 http://mashable.c~       729
    ## # ... with 5,889 more rows,
    ## #   and 60 more variables:
    ## #   n_tokens_title <dbl>,
    ## #   n_tokens_content <dbl>,
    ## #   n_unique_tokens <dbl>,
    ## #   n_non_stop_words <dbl>,
    ## #   n_non_stop_unique_tokens <dbl>, ...

## Summarizations on train set

-   descriptive statistics:

``` r
summary(train %>% select(timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_imgs, num_videos, average_token_length, num_keywords, self_reference_avg_sharess, self_reference_min_shares, self_reference_max_shares, global_rate_negative_words, global_rate_positive_words, global_sentiment_polarity, global_subjectivity, rate_negative_words, rate_positive_words, title_subjectivity, title_sentiment_polarity, abs_title_sentiment_polarity, abs_title_subjectivity))
```

    ##    timedelta    
    ##  Min.   :  8.0  
    ##  1st Qu.:107.0  
    ##  Median :240.0  
    ##  Mean   :284.4  
    ##  3rd Qu.:435.0  
    ##  Max.   :731.0  
    ##  n_tokens_title 
    ##  Min.   : 4.00  
    ##  1st Qu.: 9.00  
    ##  Median :11.00  
    ##  Mean   :10.59  
    ##  3rd Qu.:12.00  
    ##  Max.   :23.00  
    ##  n_tokens_content
    ##  Min.   :   0.0  
    ##  1st Qu.: 332.0  
    ##  Median : 506.0  
    ##  Mean   : 594.8  
    ##  3rd Qu.: 763.0  
    ##  Max.   :7081.0  
    ##  n_unique_tokens 
    ##  Min.   :0.0000  
    ##  1st Qu.:0.4656  
    ##  Median :0.5203  
    ##  Mean   :0.5098  
    ##  3rd Qu.:0.5749  
    ##  Max.   :0.9762  
    ##  n_non_stop_unique_tokens
    ##  Min.   :0.0000          
    ##  1st Qu.:0.6286          
    ##  Median :0.6841          
    ##  Mean   :0.6642          
    ##  3rd Qu.:0.7351          
    ##  Max.   :1.0000          
    ##    num_hrefs     
    ##  Min.   :  0.00  
    ##  1st Qu.:  5.00  
    ##  Median :  8.00  
    ##  Mean   : 10.19  
    ##  3rd Qu.: 13.00  
    ##  Max.   :161.00  
    ##  num_self_hrefs  
    ##  Min.   : 0.000  
    ##  1st Qu.: 1.000  
    ##  Median : 2.000  
    ##  Mean   : 2.402  
    ##  3rd Qu.: 3.000  
    ##  Max.   :38.000  
    ##     num_imgs      
    ##  Min.   :  0.000  
    ##  1st Qu.:  1.000  
    ##  Median :  1.000  
    ##  Mean   :  2.852  
    ##  3rd Qu.:  2.000  
    ##  Max.   :100.000  
    ##    num_videos     
    ##  Min.   : 0.0000  
    ##  1st Qu.: 0.0000  
    ##  Median : 0.0000  
    ##  Mean   : 0.5372  
    ##  3rd Qu.: 1.0000  
    ##  Max.   :50.0000  
    ##  average_token_length
    ##  Min.   :0.000       
    ##  1st Qu.:4.651       
    ##  Median :4.823       
    ##  Mean   :4.670       
    ##  3rd Qu.:4.976       
    ##  Max.   :6.124       
    ##   num_keywords   
    ##  Min.   : 2.000  
    ##  1st Qu.: 6.000  
    ##  Median : 7.000  
    ##  Mean   : 7.277  
    ##  3rd Qu.: 9.000  
    ##  Max.   :10.000  
    ##  self_reference_avg_sharess
    ##  Min.   :     0.0          
    ##  1st Qu.:   641.8          
    ##  Median :  1400.0          
    ##  Mean   :  4175.4          
    ##  3rd Qu.:  3100.0          
    ##  Max.   :690400.0          
    ##  self_reference_min_shares
    ##  Min.   :     0.0         
    ##  1st Qu.:   451.5         
    ##  Median :   968.0         
    ##  Mean   :  2937.9         
    ##  3rd Qu.:  1800.0         
    ##  Max.   :690400.0         
    ##  self_reference_max_shares
    ##  Min.   :     0           
    ##  1st Qu.:   660           
    ##  Median :  1700           
    ##  Mean   :  6124           
    ##  3rd Qu.:  4400           
    ##  Max.   :690400           
    ##  global_rate_negative_words
    ##  Min.   :0.00000           
    ##  1st Qu.:0.01092           
    ##  Median :0.01634           
    ##  Mean   :0.01695           
    ##  3rd Qu.:0.02222           
    ##  Max.   :0.07504           
    ##  global_rate_positive_words
    ##  Min.   :0.00000           
    ##  1st Qu.:0.02174           
    ##  Median :0.03058           
    ##  Mean   :0.03124           
    ##  3rd Qu.:0.03976           
    ##  Max.   :0.11273           
    ##  global_sentiment_polarity
    ##  Min.   :-0.35947         
    ##  1st Qu.: 0.02166         
    ##  Median : 0.07292         
    ##  Mean   : 0.07639         
    ##  3rd Qu.: 0.12622         
    ##  Max.   : 0.52000         
    ##  global_subjectivity
    ##  Min.   :0.0000     
    ##  1st Qu.:0.3568     
    ##  Median :0.4131     
    ##  Mean   :0.4024     
    ##  3rd Qu.:0.4657     
    ##  Max.   :0.9500     
    ##  rate_negative_words
    ##  Min.   :0.0000     
    ##  1st Qu.:0.2500     
    ##  Median :0.3448     
    ##  Mean   :0.3454     
    ##  3rd Qu.:0.4474     
    ##  Max.   :1.0000     
    ##  rate_positive_words
    ##  Min.   :0.0000     
    ##  1st Qu.:0.5357     
    ##  Median :0.6429     
    ##  Mean   :0.6219     
    ##  3rd Qu.:0.7391     
    ##  Max.   :1.0000     
    ##  title_subjectivity
    ##  Min.   :0.0000    
    ##  1st Qu.:0.0000    
    ##  Median :0.0000    
    ##  Mean   :0.2421    
    ##  3rd Qu.:0.4545    
    ##  Max.   :1.0000    
    ##  title_sentiment_polarity
    ##  Min.   :-1.00000        
    ##  1st Qu.: 0.00000        
    ##  Median : 0.00000        
    ##  Mean   : 0.03156        
    ##  3rd Qu.: 0.06818        
    ##  Max.   : 1.00000        
    ##  abs_title_sentiment_polarity
    ##  Min.   :0.000               
    ##  1st Qu.:0.000               
    ##  Median :0.000               
    ##  Mean   :0.127               
    ##  3rd Qu.:0.200               
    ##  Max.   :1.000               
    ##  abs_title_subjectivity
    ##  Min.   :0.000         
    ##  1st Qu.:0.200         
    ##  Median :0.500         
    ##  Mean   :0.361         
    ##  3rd Qu.:0.500         
    ##  Max.   :0.500

-   Correlation between predictors

``` r
library(corrplot)
Correlation <- cor(train %>% select(timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_imgs, num_videos, average_token_length, num_keywords, self_reference_avg_sharess, self_reference_min_shares, self_reference_max_shares, global_rate_negative_words, global_rate_positive_words, global_sentiment_polarity, global_subjectivity, rate_negative_words, rate_positive_words, title_subjectivity, title_sentiment_polarity, abs_title_sentiment_polarity, abs_title_subjectivity))
corrplot(Correlation, type="upper", tl.pos="lt")
```

![](C:/NCSU/Git/ST558_Project2/documents/world_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

This plot help us to compare correlation between predictors.

-   summary across different day of the week

We are going to create a new variable named `weekday` and show share
performance on different day of the week.

``` r
# create predictor weekday 
train_day <- train %>% mutate(weekday=ifelse(weekday_is_monday==1, "Monday", ifelse(weekday_is_tuesday==1, "Tuesday", ifelse(weekday_is_wednesday==1, "Wednesday", ifelse(weekday_is_thursday==1, "Thursday", ifelse(weekday_is_friday==1, "Friday", ifelse(weekday_is_saturday==1, "Saturday", ifelse(weekday_is_sunday==1, "Sunday", NA))))))))

# shares on different day
train_day %>% group_by(weekday) %>% summarize(n=n(), min=min(shares), max=max(shares), avg=mean(shares), median=median(shares))
```

    ## # A tibble: 7 x 6
    ##   weekday       n   min    max
    ##   <chr>     <int> <dbl>  <dbl>
    ## 1 Friday      928    35 128500
    ## 2 Monday      951    97 141400
    ## 3 Saturday    372    43  75500
    ## 4 Sunday      379    89  27300
    ## 5 Thursday   1091    42 284700
    ## 6 Tuesday    1077    42 115700
    ## 7 Wednesday  1101    48  49800
    ## # ... with 2 more variables:
    ## #   avg <dbl>, median <dbl>

We can inspect the number of records on each day as well as the minimum,
maximum, mean and median of shares on each day of the week from above
table.

Now let’s look at the count of shares on different day of the week.

``` r
g <- ggplot(train_day %>% filter(shares<quantile(shares, p=0.75)), aes(x=shares))
g + geom_freqpoly(aes(color=weekday))
```

![](C:/NCSU/Git/ST558_Project2/documents/world_files/figure-gfm/unnamed-chunk-7-1.png)

-   Number of links to other articles published by Mashable

``` r
g <- ggplot(train_day, aes(x=num_self_hrefs, y=shares) )
g + geom_point()
```

![](C:/NCSU/Git/ST558_Project2/documents/world_files/figure-gfm/unnamed-chunk-8-1.png)
