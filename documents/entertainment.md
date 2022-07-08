ST 558 Project 2
================
John Hinic & Fang Wu
2022-07-01

-   [Introduction](#introduction)
-   [Prepare Data](#prepare-data)
-   [Summarizations on train set](#summarizations-on-train-set)
-   [Modeling](#modeling)

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

-   We are going to focus on the following predictors:

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

In order to predict the number of share, we are going to build linear
regression and ensemble tree-based model.

## Prepare Data

We’ll use the `readr` and `dplyr` packages from `tifyverse`. First, we
are going to read in data as tibble using function `read_csv`. Next, in
order to access different data channel of interest automatically, we are
going to create a variable called `type`. Last we `filter` the data
channel of interest using `params$` automatically.

-   Read in raw data and create new variable `type`

``` r
# read in raw data
raw_data <- read_csv("../Data/OnlineNewsPopularity.csv") 

# create type column for different data channel
type_data <- raw_data %>% mutate(type=ifelse(data_channel_is_lifestyle==1, "lifestyle", ifelse(data_channel_is_entertainment==1, "entertainment", ifelse(data_channel_is_bus==1, "bus", ifelse(data_channel_is_socmed==1, "socmed", ifelse(data_channel_is_tech==1, "tech", ifelse(data_channel_is_world==1, "world", NA)))))))
```

-   Subset data channel of interest automatically with `params`

``` r
# select data for data channel of interest
target_data <- type_data %>% filter(type == params$filter_type) 
target_data
```

    ## # A tibble: 7,057 x 62
    ##    url            timedelta n_tokens_title
    ##    <chr>              <dbl>          <dbl>
    ##  1 http://mashab~       731             12
    ##  2 http://mashab~       731              9
    ##  3 http://mashab~       731             14
    ##  4 http://mashab~       731             12
    ##  5 http://mashab~       731             11
    ##  6 http://mashab~       731             12
    ##  7 http://mashab~       731              5
    ##  8 http://mashab~       730             11
    ##  9 http://mashab~       730             10
    ## 10 http://mashab~       729             10
    ## # ... with 7,047 more rows, and 59 more
    ## #   variables: n_tokens_content <dbl>,
    ## #   n_unique_tokens <dbl>,
    ## #   n_non_stop_words <dbl>,
    ## #   n_non_stop_unique_tokens <dbl>,
    ## #   num_hrefs <dbl>,
    ## #   num_self_hrefs <dbl>, ...

-   Split data into train and test sets

``` r
set.seed(100)
train_index <- createDataPartition(target_data$is_weekend, p=0.7, list=FALSE)
train <- target_data[train_index,]
test <- target_data[-train_index, ]
dim(train)
```

    ## [1] 4940   62

## Summarizations on train set

-   descriptive statistics on numeric variables:

``` r
summary(train %>% select(timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_imgs, num_videos, average_token_length, num_keywords, self_reference_avg_sharess, self_reference_min_shares, self_reference_max_shares, global_rate_negative_words, global_rate_positive_words, global_sentiment_polarity, global_subjectivity, rate_negative_words, rate_positive_words, title_subjectivity, title_sentiment_polarity, abs_title_sentiment_polarity, abs_title_subjectivity))
```

    ##    timedelta     n_tokens_title
    ##  Min.   :  8.0   Min.   : 3    
    ##  1st Qu.:147.0   1st Qu.:10    
    ##  Median :305.0   Median :11    
    ##  Mean   :333.1   Mean   :11    
    ##  3rd Qu.:520.0   3rd Qu.:12    
    ##  Max.   :731.0   Max.   :18    
    ##  n_tokens_content n_unique_tokens   
    ##  Min.   :   0.0   Min.   :  0.0000  
    ##  1st Qu.: 252.0   1st Qu.:  0.4703  
    ##  Median : 428.0   Median :  0.5426  
    ##  Mean   : 603.1   Mean   :  0.6747  
    ##  3rd Qu.: 796.0   3rd Qu.:  0.6182  
    ##  Max.   :6505.0   Max.   :701.0000  
    ##  n_non_stop_unique_tokens
    ##  Min.   :  0.0000        
    ##  1st Qu.:  0.6231        
    ##  Median :  0.6911        
    ##  Mean   :  0.8031        
    ##  3rd Qu.:  0.7611        
    ##  Max.   :650.0000        
    ##    num_hrefs      num_self_hrefs  
    ##  Min.   :  0.00   Min.   : 0.000  
    ##  1st Qu.:  4.00   1st Qu.: 1.000  
    ##  Median :  7.00   Median : 3.000  
    ##  Mean   : 10.78   Mean   : 3.502  
    ##  3rd Qu.: 14.00   3rd Qu.: 5.000  
    ##  Max.   :187.00   Max.   :36.000  
    ##     num_imgs         num_videos    
    ##  Min.   :  0.000   Min.   : 0.000  
    ##  1st Qu.:  1.000   1st Qu.: 0.000  
    ##  Median :  1.000   Median : 1.000  
    ##  Mean   :  6.316   Mean   : 2.506  
    ##  3rd Qu.:  8.000   3rd Qu.: 1.000  
    ##  Max.   :101.000   Max.   :74.000  
    ##  average_token_length  num_keywords   
    ##  Min.   :0.000        Min.   : 2.000  
    ##  1st Qu.:4.426        1st Qu.: 5.000  
    ##  Median :4.583        Median : 7.000  
    ##  Mean   :4.476        Mean   : 6.945  
    ##  3rd Qu.:4.754        3rd Qu.: 8.000  
    ##  Max.   :7.696        Max.   :10.000  
    ##  self_reference_avg_sharess
    ##  Min.   :     0            
    ##  1st Qu.:  1088            
    ##  Median :  2086            
    ##  Mean   :  4850            
    ##  3rd Qu.:  4844            
    ##  Max.   :143100            
    ##  self_reference_min_shares
    ##  Min.   :     0           
    ##  1st Qu.:   702           
    ##  Median :  1100           
    ##  Mean   :  2674           
    ##  3rd Qu.:  2000           
    ##  Max.   :143100           
    ##  self_reference_max_shares
    ##  Min.   :     0           
    ##  1st Qu.:  1200           
    ##  Median :  2700           
    ##  Mean   :  8512           
    ##  3rd Qu.:  9100           
    ##  Max.   :837700           
    ##  global_rate_negative_words
    ##  Min.   :0.00000           
    ##  1st Qu.:0.01060           
    ##  Median :0.01709           
    ##  Mean   :0.01881           
    ##  3rd Qu.:0.02469           
    ##  Max.   :0.09358           
    ##  global_rate_positive_words
    ##  Min.   :0.00000           
    ##  1st Qu.:0.02963           
    ##  Median :0.04048           
    ##  Mean   :0.04044           
    ##  3rd Qu.:0.05119           
    ##  Max.   :0.15278           
    ##  global_sentiment_polarity
    ##  Min.   :-0.37766         
    ##  1st Qu.: 0.04881         
    ##  Median : 0.11209         
    ##  Mean   : 0.11175         
    ##  3rd Qu.: 0.17154         
    ##  Max.   : 0.72784         
    ##  global_subjectivity rate_negative_words
    ##  Min.   :0.0000      Min.   :0.0000     
    ##  1st Qu.:0.4128      1st Qu.:0.2000     
    ##  Median :0.4642      Median :0.2989     
    ##  Mean   :0.4531      Mean   :0.3027     
    ##  3rd Qu.:0.5144      3rd Qu.:0.4000     
    ##  Max.   :1.0000      Max.   :1.0000     
    ##  rate_positive_words title_subjectivity
    ##  Min.   :0.0000      Min.   :0.0000    
    ##  1st Qu.:0.5833      1st Qu.:0.0000    
    ##  Median :0.6923      Median :0.2889    
    ##  Mean   :0.6681      Mean   :0.3126    
    ##  3rd Qu.:0.7857      3rd Qu.:0.5000    
    ##  Max.   :1.0000      Max.   :1.0000    
    ##  title_sentiment_polarity
    ##  Min.   :-1.00000        
    ##  1st Qu.: 0.00000        
    ##  Median : 0.00000        
    ##  Mean   : 0.06414        
    ##  3rd Qu.: 0.16667        
    ##  Max.   : 1.00000        
    ##  abs_title_sentiment_polarity
    ##  Min.   :0.00000             
    ##  1st Qu.:0.00000             
    ##  Median :0.06818             
    ##  Mean   :0.16890             
    ##  3rd Qu.:0.29167             
    ##  Max.   :1.00000             
    ##  abs_title_subjectivity
    ##  Min.   :0.0000        
    ##  1st Qu.:0.1250        
    ##  Median :0.4000        
    ##  Mean   :0.3232        
    ##  3rd Qu.:0.5000        
    ##  Max.   :0.5000

We can find the minimum, 25% percentile, mean, median, 75% percentile
and maximum values of each numeric variables from this chart.

``` r
sapply(train %>% select(timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_imgs, num_videos, average_token_length, num_keywords, self_reference_avg_sharess, self_reference_min_shares, self_reference_max_shares, global_rate_negative_words, global_rate_positive_words, global_sentiment_polarity, global_subjectivity, rate_negative_words, rate_positive_words, title_subjectivity, title_sentiment_polarity, abs_title_sentiment_polarity, abs_title_subjectivity), sd)
```

    ##                    timedelta 
    ##                 2.096288e+02 
    ##               n_tokens_title 
    ##                 2.092896e+00 
    ##             n_tokens_content 
    ##                 5.310665e+02 
    ##              n_unique_tokens 
    ##                 9.967041e+00 
    ##     n_non_stop_unique_tokens 
    ##                 9.239835e+00 
    ##                    num_hrefs 
    ##                 1.287618e+01 
    ##               num_self_hrefs 
    ##                 3.136078e+00 
    ##                     num_imgs 
    ##                 1.147419e+01 
    ##                   num_videos 
    ##                 6.185936e+00 
    ##         average_token_length 
    ##                 8.150547e-01 
    ##                 num_keywords 
    ##                 1.914197e+00 
    ##   self_reference_avg_sharess 
    ##                 9.570805e+03 
    ##    self_reference_min_shares 
    ##                 6.894799e+03 
    ##    self_reference_max_shares 
    ##                 2.343502e+04 
    ##   global_rate_negative_words 
    ##                 1.225443e-02 
    ##   global_rate_positive_words 
    ##                 1.685006e-02 
    ##    global_sentiment_polarity 
    ##                 9.988602e-02 
    ##          global_subjectivity 
    ##                 1.134283e-01 
    ##          rate_negative_words 
    ##                 1.544505e-01 
    ##          rate_positive_words 
    ##                 1.857673e-01 
    ##           title_subjectivity 
    ##                 3.261990e-01 
    ##     title_sentiment_polarity 
    ##                 2.753137e-01 
    ## abs_title_sentiment_polarity 
    ##                 2.266685e-01 
    ##       abs_title_subjectivity 
    ##                 1.922774e-01

From here we can compare standard deviation between numeric variables.

-   Correlation between numeric predictors

``` r
Correlation <- cor(train %>% select(timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_imgs, num_videos, average_token_length, num_keywords, self_reference_avg_sharess, self_reference_min_shares, self_reference_max_shares, global_rate_negative_words, global_rate_positive_words, global_sentiment_polarity, global_subjectivity, rate_negative_words, rate_positive_words, title_subjectivity, title_sentiment_polarity, abs_title_sentiment_polarity, abs_title_subjectivity))
corrplot(Correlation, type="upper", tl.pos="lt", cl.cex=0.8)
```

![](C:/NCSU/Git/ST558_Project2/documents/entertainment_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

This plot help us to check linear relationship between predictors. We
want to avoid include predictors with high correlation in the same
model.

-   summary across different day of the week

We are going to create a new variable named `weekday` and visualize
share performance on different day of the week.

``` r
# create predictor weekday 
train <- train %>% mutate(weekday=ifelse(weekday_is_monday==1, "Monday", ifelse(weekday_is_tuesday==1, "Tuesday", ifelse(weekday_is_wednesday==1, "Wednesday", ifelse(weekday_is_thursday==1, "Thursday", ifelse(weekday_is_friday==1, "Friday", ifelse(weekday_is_saturday==1, "Saturday", ifelse(weekday_is_sunday==1, "Sunday", NA))))))))

test <- test %>% mutate(weekday=ifelse(weekday_is_monday==1, "Monday", ifelse(weekday_is_tuesday==1, "Tuesday", ifelse(weekday_is_wednesday==1, "Wednesday", ifelse(weekday_is_thursday==1, "Thursday", ifelse(weekday_is_friday==1, "Friday", ifelse(weekday_is_saturday==1, "Saturday", ifelse(weekday_is_sunday==1, "Sunday", NA))))))))

# shares on different day
train %>% group_by(weekday) %>% summarize(n=n(), min=min(shares), max=max(shares), avg=mean(shares), median=median(shares))
```

    ## # A tibble: 7 x 6
    ##   weekday      n   min    max   avg median
    ##   <chr>    <int> <dbl>  <dbl> <dbl>  <dbl>
    ## 1 Friday     705    82 210300 2888.   1200
    ## 2 Monday     937    59  77200 2581.   1100
    ## 3 Saturday   268    65  68300 3701.   1600
    ## 4 Sunday     379   171  69500 3852.   1700
    ## 5 Thursday   822    57 197600 2877.   1100
    ## 6 Tuesday    903    47  98000 2702.   1100
    ## 7 Wednesd~   926    49 109500 3018.   1100

We can inspect the effect of `weekday` on the `share`. The number of
records on each day as well as the minimum, maximum, mean and median
values of shares on each day of the week are included in the table here.
If there are big difference across `weekday`, then `weekday` and `share`
are dependent.

We also can check the difference in plot.

``` r
g <- ggplot(train %>% filter(shares<quantile(shares, p=0.75)), aes(x=shares))
g + geom_freqpoly(aes(color=weekday))
```

![](C:/NCSU/Git/ST558_Project2/documents/entertainment_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

-   Scatter plot

We want to check the relationship between response variable `share` and
other predictors through scatter plot. Linear or non-linear? Positive or
negative?

``` r
g <- ggplot(train, aes(x=num_self_hrefs, y=shares, col=weekday) )
g + geom_point()
```

![](C:/NCSU/Git/ST558_Project2/documents/entertainment_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
g <- ggplot(train, aes(x=num_imgs, y=shares, col=weekday) )
g + geom_point()
```

![](C:/NCSU/Git/ST558_Project2/documents/entertainment_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
g <- ggplot(train, aes(x=rate_positive_words, y=shares, col=weekday) )
g + geom_point()
```

![](C:/NCSU/Git/ST558_Project2/documents/entertainment_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Modeling

### Linear Regression

``` r
mlFit <- train(shares~timedelta+weekday+num_self_hrefs+num_imgs+num_videos+rate_positive_words, data=train, method="lm", preProcess=c("center", "scale"), trControl=trainControl(method="cv", number=10))
mlFit
```

    ## Linear Regression 
    ## 
    ## 4940 samples
    ##    6 predictor
    ## 
    ## Pre-processing: centered (11), scaled (11) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 4447, 4446, 4446, 4446, 4445, 4446, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared     MAE     
    ##   7622.548  0.003566227  2889.731
    ## 
    ## Tuning parameter 'intercept' was
    ##  held constant at a value of TRUE

### Tree-based model

-   Boosted Trees

Boosted trees model trains a bunch of trees sequentially. Each
subsequent tree learns from the mistakes of the previous tree. So
predictions get updated as trees grown. It is used for both regression
and classification.

``` r
n.trees=c(50, 100, 150)
interaction.depth=c(2,3,4)
shrinkage=c(0.1, 0.5)
n.minobsinnode=c(10)
tune_parameter <- expand.grid(n.trees=n.trees, interaction.depth=interaction.depth, shrinkage=shrinkage, n.minobsinnode=n.minobsinnode)
boostedFit <- train(shares~timedelta+weekday+num_self_hrefs+num_imgs+num_videos+rate_positive_words, data=train, method="gbm", trControl=trainControl(method="repeatedcv", number=5, repeats=3), tuneGrid=tune_parameter)
boostedFit
```

### Compare models on the test set

``` r
ml_pred <- predict(mlFit, test)
ml_MSE <- postResample(test$shares, ml_pred)[1]
boosted_pred <- predict(boostedFit, test)
boosted_MSE <- postResample(test$shares, boosted_pred)[1]
comp <- data.frame(LR=ml_MSE, Boosted=boosted_MSE)
comp
```

    ##            LR Boosted
    ## RMSE 7505.419 7469.46

``` r
best_model <- which.min(comp["RMSE",])
best_model
```

    ## Boosted 
    ##       2

Boosted has the minimum MSE which indicates the best fitting.
