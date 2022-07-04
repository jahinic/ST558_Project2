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

    ## # A tibble: 2,099 x 62
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
    ##  9 http://mashable.c~       730
    ## 10 http://mashable.c~       729
    ## # ... with 2,089 more rows,
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

    ## # A tibble: 1,470 x 62
    ##    url                timedelta
    ##    <chr>                  <dbl>
    ##  1 http://mashable.c~       731
    ##  2 http://mashable.c~       731
    ##  3 http://mashable.c~       731
    ##  4 http://mashable.c~       731
    ##  5 http://mashable.c~       731
    ##  6 http://mashable.c~       731
    ##  7 http://mashable.c~       729
    ##  8 http://mashable.c~       729
    ##  9 http://mashable.c~       729
    ## 10 http://mashable.c~       729
    ## # ... with 1,460 more rows,
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
    ##  Min.   :  9.0  
    ##  1st Qu.:211.2  
    ##  Median :413.5  
    ##  Mean   :405.1  
    ##  3rd Qu.:613.8  
    ##  Max.   :731.0  
    ##  n_tokens_title  
    ##  Min.   : 3.000  
    ##  1st Qu.: 8.000  
    ##  Median :10.000  
    ##  Mean   : 9.741  
    ##  3rd Qu.:11.000  
    ##  Max.   :18.000  
    ##  n_tokens_content
    ##  Min.   :   0.0  
    ##  1st Qu.: 306.2  
    ##  Median : 505.0  
    ##  Mean   : 623.4  
    ##  3rd Qu.: 802.8  
    ##  Max.   :7764.0  
    ##  n_unique_tokens 
    ##  Min.   :0.0000  
    ##  1st Qu.:0.4616  
    ##  Median :0.5187  
    ##  Mean   :0.5219  
    ##  3rd Qu.:0.5907  
    ##  Max.   :0.8681  
    ##  n_non_stop_unique_tokens
    ##  Min.   :0.0000          
    ##  1st Qu.:0.6251          
    ##  Median :0.6849          
    ##  Mean   :0.6818          
    ##  3rd Qu.:0.7527          
    ##  Max.   :0.9697          
    ##    num_hrefs     
    ##  Min.   :  0.00  
    ##  1st Qu.:  6.00  
    ##  Median : 10.00  
    ##  Mean   : 13.37  
    ##  3rd Qu.: 18.00  
    ##  Max.   :145.00  
    ##  num_self_hrefs  
    ##  Min.   : 0.000  
    ##  1st Qu.: 1.000  
    ##  Median : 2.000  
    ##  Mean   : 2.467  
    ##  3rd Qu.: 3.000  
    ##  Max.   :40.000  
    ##     num_imgs      
    ##  Min.   :  0.000  
    ##  1st Qu.:  1.000  
    ##  Median :  1.000  
    ##  Mean   :  4.836  
    ##  3rd Qu.:  8.000  
    ##  Max.   :111.000  
    ##    num_videos     
    ##  Min.   : 0.0000  
    ##  1st Qu.: 0.0000  
    ##  Median : 0.0000  
    ##  Mean   : 0.5143  
    ##  3rd Qu.: 0.0000  
    ##  Max.   :50.0000  
    ##  average_token_length
    ##  Min.   :0.000       
    ##  1st Qu.:4.441       
    ##  Median :4.614       
    ##  Mean   :4.574       
    ##  3rd Qu.:4.793       
    ##  Max.   :5.813       
    ##   num_keywords   
    ##  Min.   : 3.000  
    ##  1st Qu.: 7.000  
    ##  Median : 8.000  
    ##  Mean   : 8.239  
    ##  3rd Qu.:10.000  
    ##  Max.   :10.000  
    ##  self_reference_avg_sharess
    ##  Min.   :     0.0          
    ##  1st Qu.:   919.5          
    ##  Median :  2500.0          
    ##  Mean   :  6227.8          
    ##  3rd Qu.:  5700.0          
    ##  Max.   :401450.0          
    ##  self_reference_min_shares
    ##  Min.   :     0.0         
    ##  1st Qu.:   625.2         
    ##  Median :  1600.0         
    ##  Mean   :  4594.4         
    ##  3rd Qu.:  3800.0         
    ##  Max.   :144900.0         
    ##  self_reference_max_shares
    ##  Min.   :     0.0         
    ##  1st Qu.:   919.5         
    ##  Median :  2800.0         
    ##  Mean   :  8489.6         
    ##  3rd Qu.:  7300.0         
    ##  Max.   :690400.0         
    ##  global_rate_negative_words
    ##  Min.   :0.00000           
    ##  1st Qu.:0.01003           
    ##  Median :0.01520           
    ##  Mean   :0.01615           
    ##  3rd Qu.:0.02085           
    ##  Max.   :0.06061           
    ##  global_rate_positive_words
    ##  Min.   :0.00000           
    ##  1st Qu.:0.03483           
    ##  Median :0.04396           
    ##  Mean   :0.04436           
    ##  3rd Qu.:0.05333           
    ##  Max.   :0.12139           
    ##  global_sentiment_polarity
    ##  Min.   :-0.3727          
    ##  1st Qu.: 0.1027          
    ##  Median : 0.1509          
    ##  Mean   : 0.1524          
    ##  3rd Qu.: 0.2048          
    ##  Max.   : 0.5800          
    ##  global_subjectivity
    ##  Min.   :0.0000     
    ##  1st Qu.:0.4270     
    ##  Median :0.4782     
    ##  Mean   :0.4738     
    ##  3rd Qu.:0.5251     
    ##  Max.   :0.8667     
    ##  rate_negative_words
    ##  Min.   :0.0000     
    ##  1st Qu.:0.1818     
    ##  Median :0.2540     
    ##  Mean   :0.2632     
    ##  3rd Qu.:0.3333     
    ##  Max.   :1.0000     
    ##  rate_positive_words
    ##  Min.   :0.0000     
    ##  1st Qu.:0.6667     
    ##  Median :0.7407     
    ##  Mean   :0.7239     
    ##  3rd Qu.:0.8125     
    ##  Max.   :1.0000     
    ##  title_subjectivity
    ##  Min.   :0.0000    
    ##  1st Qu.:0.0000    
    ##  Median :0.1000    
    ##  Mean   :0.2831    
    ##  3rd Qu.:0.5000    
    ##  Max.   :1.0000    
    ##  title_sentiment_polarity
    ##  Min.   :-1.0000         
    ##  1st Qu.: 0.0000         
    ##  Median : 0.0000         
    ##  Mean   : 0.1072         
    ##  3rd Qu.: 0.2138         
    ##  Max.   : 1.0000         
    ##  abs_title_sentiment_polarity
    ##  Min.   :0.0000              
    ##  1st Qu.:0.0000              
    ##  Median :0.0000              
    ##  Mean   :0.1720              
    ##  3rd Qu.:0.2733              
    ##  Max.   :1.0000              
    ##  abs_title_subjectivity
    ##  Min.   :0.0000        
    ##  1st Qu.:0.1667        
    ##  Median :0.5000        
    ##  Mean   :0.3453        
    ##  3rd Qu.:0.5000        
    ##  Max.   :0.5000

-   Correlation between predictors

``` r
library(corrplot)
Correlation <- cor(train %>% select(timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_imgs, num_videos, average_token_length, num_keywords, self_reference_avg_sharess, self_reference_min_shares, self_reference_max_shares, global_rate_negative_words, global_rate_positive_words, global_sentiment_polarity, global_subjectivity, rate_negative_words, rate_positive_words, title_subjectivity, title_sentiment_polarity, abs_title_sentiment_polarity, abs_title_subjectivity))
corrplot(Correlation, type="upper", tl.pos="lt")
```

![](C:/NCSU/Git/ST558_Project2/documents/lifestyle_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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
    ## 1 Friday      221   127  40400
    ## 2 Monday      218   171 196700
    ## 3 Saturday    133   776  29200
    ## 4 Sunday      152   613  27500
    ## 5 Thursday    260   184  56000
    ## 6 Tuesday     227    93  54900
    ## 7 Wednesday   259    78  73100
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

![](C:/NCSU/Git/ST558_Project2/documents/lifestyle_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

-   Number of links to other articles published by Mashable

``` r
g <- ggplot(train_day, aes(x=num_self_hrefs, y=shares) )
g + geom_point()
```

![](C:/NCSU/Git/ST558_Project2/documents/lifestyle_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
