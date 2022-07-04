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

    ## # A tibble: 2,323 x 62
    ##    url               timedelta n_tokens_title
    ##    <chr>                 <dbl>          <dbl>
    ##  1 http://mashable.~       731              8
    ##  2 http://mashable.~       731              8
    ##  3 http://mashable.~       731              9
    ##  4 http://mashable.~       731             10
    ##  5 http://mashable.~       729              9
    ##  6 http://mashable.~       729              9
    ##  7 http://mashable.~       729             10
    ##  8 http://mashable.~       728              7
    ##  9 http://mashable.~       727              8
    ## 10 http://mashable.~       727              6
    ## # ... with 2,313 more rows, and 59 more
    ## #   variables: n_tokens_content <dbl>,
    ## #   n_unique_tokens <dbl>,
    ## #   n_non_stop_words <dbl>,
    ## #   n_non_stop_unique_tokens <dbl>,
    ## #   num_hrefs <dbl>, num_self_hrefs <dbl>,
    ## #   num_imgs <dbl>, num_videos <dbl>, ...

-   Split data into train and test sets

``` r
library(caret)
set.seed(100)
train_index <- createDataPartition(target_data$is_weekend, p=0.7, list=FALSE)
train <- target_data[train_index,]
test <- target_data[-train_index, ]
train
```

    ## # A tibble: 1,627 x 62
    ##    url               timedelta n_tokens_title
    ##    <chr>                 <dbl>          <dbl>
    ##  1 http://mashable.~       731              8
    ##  2 http://mashable.~       731              8
    ##  3 http://mashable.~       731              9
    ##  4 http://mashable.~       731             10
    ##  5 http://mashable.~       729              9
    ##  6 http://mashable.~       729              9
    ##  7 http://mashable.~       729             10
    ##  8 http://mashable.~       727             11
    ##  9 http://mashable.~       726             13
    ## 10 http://mashable.~       725             10
    ## # ... with 1,617 more rows, and 59 more
    ## #   variables: n_tokens_content <dbl>,
    ## #   n_unique_tokens <dbl>,
    ## #   n_non_stop_words <dbl>,
    ## #   n_non_stop_unique_tokens <dbl>,
    ## #   num_hrefs <dbl>, num_self_hrefs <dbl>,
    ## #   num_imgs <dbl>, num_videos <dbl>, ...

## Summarizations on train set

-   descriptive statistics:

``` r
summary(train %>% select(timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_imgs, num_videos, average_token_length, num_keywords, self_reference_avg_sharess, self_reference_min_shares, self_reference_max_shares, global_rate_negative_words, global_rate_positive_words, global_sentiment_polarity, global_subjectivity, rate_negative_words, rate_positive_words, title_subjectivity, title_sentiment_polarity, abs_title_sentiment_polarity, abs_title_subjectivity))
```

    ##    timedelta     n_tokens_title  
    ##  Min.   : 11.0   Min.   : 4.000  
    ##  1st Qu.:259.5   1st Qu.: 8.000  
    ##  Median :429.0   Median : 9.000  
    ##  Mean   :420.4   Mean   : 9.555  
    ##  3rd Qu.:597.0   3rd Qu.:11.000  
    ##  Max.   :731.0   Max.   :18.000  
    ##  n_tokens_content n_unique_tokens 
    ##  Min.   :   0.0   Min.   :0.0000  
    ##  1st Qu.: 250.5   1st Qu.:0.4635  
    ##  Median : 434.0   Median :0.5366  
    ##  Mean   : 607.9   Mean   :0.5372  
    ##  3rd Qu.: 764.0   3rd Qu.:0.6080  
    ##  Max.   :3735.0   Max.   :0.9714  
    ##  n_non_stop_unique_tokens   num_hrefs     
    ##  Min.   :0.0000           Min.   :  0.00  
    ##  1st Qu.:0.6172           1st Qu.:  5.00  
    ##  Median :0.6830           Median :  8.00  
    ##  Mean   :0.6833           Mean   : 13.27  
    ##  3rd Qu.:0.7559           3rd Qu.: 15.00  
    ##  Max.   :1.0000           Max.   :110.00  
    ##  num_self_hrefs      num_imgs    
    ##  Min.   : 0.000   Min.   : 0.00  
    ##  1st Qu.: 2.000   1st Qu.: 1.00  
    ##  Median : 3.000   Median : 1.00  
    ##  Mean   : 4.856   Mean   : 4.27  
    ##  3rd Qu.: 5.000   3rd Qu.: 3.00  
    ##  Max.   :74.000   Max.   :62.00  
    ##    num_videos     average_token_length
    ##  Min.   : 0.000   Min.   :0.000       
    ##  1st Qu.: 0.000   1st Qu.:4.496       
    ##  Median : 0.000   Median :4.657       
    ##  Mean   : 1.052   Mean   :4.644       
    ##  3rd Qu.: 1.000   3rd Qu.:4.813       
    ##  Max.   :34.000   Max.   :5.774       
    ##   num_keywords    self_reference_avg_sharess
    ##  Min.   : 1.000   Min.   :     0            
    ##  1st Qu.: 5.000   1st Qu.:  1506            
    ##  Median : 7.000   Median :  3300            
    ##  Mean   : 6.615   Mean   :  8963            
    ##  3rd Qu.: 8.000   3rd Qu.:  7403            
    ##  Max.   :10.000   Max.   :690400            
    ##  self_reference_min_shares
    ##  Min.   :     0.0         
    ##  1st Qu.:   779.5         
    ##  Median :  1600.0         
    ##  Mean   :  5408.6         
    ##  3rd Qu.:  3350.0         
    ##  Max.   :690400.0         
    ##  self_reference_max_shares
    ##  Min.   :     0           
    ##  1st Qu.:  1700           
    ##  Median :  4400           
    ##  Mean   : 16378           
    ##  3rd Qu.: 13100           
    ##  Max.   :690400           
    ##  global_rate_negative_words
    ##  Min.   :0.000000          
    ##  1st Qu.:0.009091          
    ##  Median :0.014286          
    ##  Mean   :0.015659          
    ##  3rd Qu.:0.020628          
    ##  Max.   :0.139831          
    ##  global_rate_positive_words
    ##  Min.   :0.00000           
    ##  1st Qu.:0.03549           
    ##  Median :0.04583           
    ##  Mean   :0.04682           
    ##  3rd Qu.:0.05677           
    ##  Max.   :0.15549           
    ##  global_sentiment_polarity
    ##  Min.   :-0.37500         
    ##  1st Qu.: 0.08963         
    ##  Median : 0.14360         
    ##  Mean   : 0.14654         
    ##  3rd Qu.: 0.19737         
    ##  Max.   : 0.56667         
    ##  global_subjectivity rate_negative_words
    ##  Min.   :0.0000      Min.   :0.0000     
    ##  1st Qu.:0.4080      1st Qu.:0.1610     
    ##  Median :0.4612      Median :0.2414     
    ##  Mean   :0.4596      Mean   :0.2486     
    ##  3rd Qu.:0.5144      3rd Qu.:0.3277     
    ##  Max.   :0.9222      Max.   :1.0000     
    ##  rate_positive_words title_subjectivity
    ##  Min.   :0.0000      Min.   :0.00000   
    ##  1st Qu.:0.6667      1st Qu.:0.00000   
    ##  Median :0.7569      Median :0.06667   
    ##  Mean   :0.7471      Mean   :0.26377   
    ##  3rd Qu.:0.8375      3rd Qu.:0.47614   
    ##  Max.   :1.0000      Max.   :1.00000   
    ##  title_sentiment_polarity
    ##  Min.   :-1.00000        
    ##  1st Qu.: 0.00000        
    ##  Median : 0.00000        
    ##  Mean   : 0.09804        
    ##  3rd Qu.: 0.15590        
    ##  Max.   : 1.00000        
    ##  abs_title_sentiment_polarity
    ##  Min.   :0.0000              
    ##  1st Qu.:0.0000              
    ##  Median :0.0000              
    ##  Mean   :0.1568              
    ##  3rd Qu.:0.2500              
    ##  Max.   :1.0000              
    ##  abs_title_subjectivity
    ##  Min.   :0.0000        
    ##  1st Qu.:0.1875        
    ##  Median :0.5000        
    ##  Mean   :0.3501        
    ##  3rd Qu.:0.5000        
    ##  Max.   :0.5000

-   Correlation between predictors

``` r
library(corrplot)
Correlation <- cor(train %>% select(timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_imgs, num_videos, average_token_length, num_keywords, self_reference_avg_sharess, self_reference_min_shares, self_reference_max_shares, global_rate_negative_words, global_rate_positive_words, global_sentiment_polarity, global_subjectivity, rate_negative_words, rate_positive_words, title_subjectivity, title_sentiment_polarity, abs_title_sentiment_polarity, abs_title_subjectivity))
corrplot(Correlation, type="upper", tl.pos="lt")
```

![](images/unnamed-chunk-5-1.png)<!-- -->

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
    ##   weekday       n   min    max   avg median
    ##   <chr>     <int> <dbl>  <dbl> <dbl>  <dbl>
    ## 1 Friday      227   213  57000 4226.   2200
    ## 2 Monday      240   200  57600 4079.   2550
    ## 3 Saturday    124   217  34500 3567.   2300
    ## 4 Sunday       92   636  54100 5273.   2750
    ## 5 Thursday    324   165  26900 3227.   2100
    ## 6 Tuesday     318   238 122800 3594.   1900
    ## 7 Wednesday   302   398  59000 3486.   2000

We can inspect the number of records on each day as well as the minimum,
maximum, mean and median of shares on each day of the week from above
table.

Now let’s look at the count of shares on different day of the week.

``` r
g <- ggplot(train_day %>% filter(shares<quantile(shares, p=0.75)), aes(x=shares))
g + geom_freqpoly(aes(color=weekday))
```

![](images/unnamed-chunk-7-1.png)<!-- -->

-   Number of links to other articles published by Mashable

``` r
g <- ggplot(train_day, aes(x=num_self_hrefs, y=shares) )
g + geom_point()
```

![](images/unnamed-chunk-8-1.png)<!-- -->
