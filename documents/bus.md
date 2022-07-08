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

-   Split data into train and test sets

``` r
set.seed(100)
train_index <- createDataPartition(target_data$is_weekend, p=0.7, list=FALSE)
train <- target_data[train_index,]
test <- target_data[-train_index, ]
dim(train)
```

    ## [1] 4381   62

## Summarizations on train set

-   descriptive statistics on numeric variables:

``` r
summary(train %>% select(timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_imgs, num_videos, average_token_length, num_keywords, self_reference_avg_sharess, self_reference_min_shares, self_reference_max_shares, global_rate_negative_words, global_rate_positive_words, global_sentiment_polarity, global_subjectivity, rate_negative_words, rate_positive_words, title_subjectivity, title_sentiment_polarity, abs_title_sentiment_polarity, abs_title_subjectivity))
```

    ##    timedelta     n_tokens_title
    ##  Min.   :  8.0   Min.   : 3.0  
    ##  1st Qu.:204.0   1st Qu.: 9.0  
    ##  Median :385.0   Median :10.0  
    ##  Mean   :381.6   Mean   :10.3  
    ##  3rd Qu.:567.0   3rd Qu.:12.0  
    ##  Max.   :731.0   Max.   :19.0  
    ##  n_tokens_content n_unique_tokens 
    ##  Min.   :   0.0   Min.   :0.0000  
    ##  1st Qu.: 245.0   1st Qu.:0.4783  
    ##  Median : 400.0   Median :0.5486  
    ##  Mean   : 538.2   Mean   :0.5462  
    ##  3rd Qu.: 729.0   3rd Qu.:0.6108  
    ##  Max.   :4747.0   Max.   :0.8732  
    ##  n_non_stop_unique_tokens
    ##  Min.   :0.0000          
    ##  1st Qu.:0.6491          
    ##  Median :0.7043          
    ##  Mean   :0.7038          
    ##  3rd Qu.:0.7603          
    ##  Max.   :0.9730          
    ##    num_hrefs       num_self_hrefs  
    ##  Min.   :  0.000   Min.   : 0.000  
    ##  1st Qu.:  4.000   1st Qu.: 1.000  
    ##  Median :  7.000   Median : 2.000  
    ##  Mean   :  9.283   Mean   : 2.764  
    ##  3rd Qu.: 11.000   3rd Qu.: 4.000  
    ##  Max.   :102.000   Max.   :56.000  
    ##     num_imgs        num_videos    
    ##  Min.   : 0.000   Min.   : 0.000  
    ##  1st Qu.: 1.000   1st Qu.: 0.000  
    ##  Median : 1.000   Median : 0.000  
    ##  Mean   : 1.786   Mean   : 0.617  
    ##  3rd Qu.: 1.000   3rd Qu.: 0.000  
    ##  Max.   :40.000   Max.   :75.000  
    ##  average_token_length  num_keywords   
    ##  Min.   :0.000        Min.   : 2.000  
    ##  1st Qu.:4.527        1st Qu.: 5.000  
    ##  Median :4.686        Median : 6.000  
    ##  Mean   :4.688        Mean   : 6.481  
    ##  3rd Qu.:4.860        3rd Qu.: 8.000  
    ##  Max.   :5.960        Max.   :10.000  
    ##  self_reference_avg_sharess
    ##  Min.   :     0            
    ##  1st Qu.:   549            
    ##  Median :  2000            
    ##  Mean   :  6017            
    ##  3rd Qu.:  4300            
    ##  Max.   :690400            
    ##  self_reference_min_shares
    ##  Min.   :     0           
    ##  1st Qu.:   164           
    ##  Median :  1100           
    ##  Mean   :  3375           
    ##  3rd Qu.:  2200           
    ##  Max.   :690400           
    ##  self_reference_max_shares
    ##  Min.   :     0           
    ##  1st Qu.:   577           
    ##  Median :  2500           
    ##  Mean   : 10212           
    ##  3rd Qu.:  6100           
    ##  Max.   :690400           
    ##  global_rate_negative_words
    ##  Min.   :0.000000          
    ##  1st Qu.:0.009091          
    ##  Median :0.014085          
    ##  Mean   :0.014750          
    ##  3rd Qu.:0.019305          
    ##  Max.   :0.064220          
    ##  global_rate_positive_words
    ##  Min.   :0.00000           
    ##  1st Qu.:0.03214           
    ##  Median :0.04235           
    ##  Mean   :0.04329           
    ##  3rd Qu.:0.05365           
    ##  Max.   :0.12500           
    ##  global_sentiment_polarity
    ##  Min.   :-0.2176          
    ##  1st Qu.: 0.0839          
    ##  Median : 0.1356          
    ##  Mean   : 0.1357          
    ##  3rd Qu.: 0.1852          
    ##  Max.   : 0.5737          
    ##  global_subjectivity rate_negative_words
    ##  Min.   :0.0000      Min.   :0.0000     
    ##  1st Qu.:0.3866      1st Qu.:0.1667     
    ##  Median :0.4409      Median :0.2500     
    ##  Mean   :0.4365      Mean   :0.2581     
    ##  3rd Qu.:0.4894      3rd Qu.:0.3333     
    ##  Max.   :1.0000      Max.   :1.0000     
    ##  rate_positive_words title_subjectivity
    ##  Min.   :0.0000      Min.   :0.0000    
    ##  1st Qu.:0.6667      1st Qu.:0.0000    
    ##  Median :0.7500      Median :0.1000    
    ##  Mean   :0.7382      Mean   :0.2491    
    ##  3rd Qu.:0.8333      3rd Qu.:0.4750    
    ##  Max.   :1.0000      Max.   :1.0000    
    ##  title_sentiment_polarity
    ##  Min.   :-1.00000        
    ##  1st Qu.: 0.00000        
    ##  Median : 0.00000        
    ##  Mean   : 0.08099        
    ##  3rd Qu.: 0.13636        
    ##  Max.   : 1.00000        
    ##  abs_title_sentiment_polarity
    ##  Min.   :0.0000              
    ##  1st Qu.:0.0000              
    ##  Median :0.0000              
    ##  Mean   :0.1396              
    ##  3rd Qu.:0.2121              
    ##  Max.   :1.0000              
    ##  abs_title_subjectivity
    ##  Min.   :0.0000        
    ##  1st Qu.:0.1648        
    ##  Median :0.5000        
    ##  Mean   :0.3398        
    ##  3rd Qu.:0.5000        
    ##  Max.   :0.5000

We can find the minimum, 25% percentile, mean, median, 75% percentile
and maximum values of each numeric variables from this chart.

``` r
sapply(train %>% select(timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_imgs, num_videos, average_token_length, num_keywords, self_reference_avg_sharess, self_reference_min_shares, self_reference_max_shares, global_rate_negative_words, global_rate_positive_words, global_sentiment_polarity, global_subjectivity, rate_negative_words, rate_positive_words, title_subjectivity, title_sentiment_polarity, abs_title_sentiment_polarity, abs_title_subjectivity), sd)
```

    ##                    timedelta 
    ##                 2.084392e+02 
    ##               n_tokens_title 
    ##                 2.156898e+00 
    ##             n_tokens_content 
    ##                 4.258133e+02 
    ##              n_unique_tokens 
    ##                 1.004417e-01 
    ##     n_non_stop_unique_tokens 
    ##                 9.472628e-02 
    ##                    num_hrefs 
    ##                 8.115511e+00 
    ##               num_self_hrefs 
    ##                 2.824893e+00 
    ##                     num_imgs 
    ##                 3.343843e+00 
    ##                   num_videos 
    ##                 3.343203e+00 
    ##         average_token_length 
    ##                 3.886922e-01 
    ##                 num_keywords 
    ##                 1.963925e+00 
    ##   self_reference_avg_sharess 
    ##                 2.678431e+04 
    ##    self_reference_min_shares 
    ##                 2.022250e+04 
    ##    self_reference_max_shares 
    ##                 4.785129e+04 
    ##   global_rate_negative_words 
    ##                 8.560499e-03 
    ##   global_rate_positive_words 
    ##                 1.635989e-02 
    ##    global_sentiment_polarity 
    ##                 8.265347e-02 
    ##          global_subjectivity 
    ##                 8.452655e-02 
    ##          rate_negative_words 
    ##                 1.375670e-01 
    ##          rate_positive_words 
    ##                 1.438000e-01 
    ##           title_subjectivity 
    ##                 2.980393e-01 
    ##     title_sentiment_polarity 
    ##                 2.389479e-01 
    ## abs_title_sentiment_polarity 
    ##                 2.101250e-01 
    ##       abs_title_subjectivity 
    ##                 1.905357e-01

From here we can compare standard deviation between numeric variables.

-   Correlation between numeric variables

``` r
#str(train)
Correlation <- cor(train %>% select(-url, -type, -starts_with("weekday"), -starts_with("data_channel"), -is_weekend ))
corrplot(Correlation, type="upper", tl.pos="lt", cl.cex=0.8)
```

![](C:/NCSU/Git/ST558_Project2/documents/bus_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

This plot help us to check linear relationship between numeric
variables. We want to avoid include predictors with high correlation in
the same model.

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
    ##   weekday     n   min    max   avg median
    ##   <chr>   <int> <dbl>  <dbl> <dbl>  <dbl>
    ## 1 Friday    572    28 102200 2302.   1400
    ## 2 Monday    803     1 690400 4381.   1400
    ## 3 Saturd~   168   150 144400 4392.   2600
    ## 4 Sunday    250   692  56900 3663.   2200
    ## 5 Thursd~   861    99 306100 3121.   1300
    ## 6 Tuesday   811    44 310800 2943.   1300
    ## 7 Wednes~   916    63 158900 2733.   1300

We can inspect the effect of `weekday` on the `share`. The number of
records on each day as well as the minimum, maximum, mean and median
values of shares on each day of the week are included in the table here.
If there are big difference across `weekday`, then `weekday` and `share`
are dependent.

We also can check the difference in plot.

``` r
g <- ggplot(train %>% filter(shares<quantile(shares, p=0.75)), aes(x=shares))
g + geom_freqpoly(aes(color=weekday)) +
  ggtitle("Counts of shares across day of the week")
```

![](C:/NCSU/Git/ST558_Project2/documents/bus_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot(train, aes(x=weekday, y=shares)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(min(train$shares), quantile(train$shares, p=0.75)+IQR(train$shares))) +
  ggtitle("box plot of shares across day of the week")
```

![](C:/NCSU/Git/ST558_Project2/documents/bus_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

In this plot, we can compare the median, 25% percentile, 75% percentile
and IQR of shares between different day of the week. It will show the
effect of day on the shares.

-   Scatter plot

We want to check the relationship between response variable `share` and
other predictors through scatter plot. Linear or non-linear? Positive or
negative?

``` r
g <- ggplot(train, aes(x=num_self_hrefs, y=shares) )
g + geom_jitter() +
    scale_y_continuous(limits=c(min(train$shares), quantile(train$shares, p=0.75)+2*IQR(train$shares))) +
    scale_x_continuous(limits=c(min(train$num_self_hrefs), quantile(train$num_self_hrefs, p=0.75)+2*IQR(train$num_self_hrefs))) +
    ggtitle("scatter plot of shares against number of links") 
```

![](C:/NCSU/Git/ST558_Project2/documents/bus_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
g <- ggplot(train, aes(x=rate_positive_words, y=shares) )
g + geom_point() +
  scale_y_continuous(limits=c(min(train$shares), quantile(train$shares, p=0.75)+2*IQR(train$shares))) +
  ggtitle("scatter plot of shares against rate of positive words")
```

![](C:/NCSU/Git/ST558_Project2/documents/bus_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Modeling

### Linear Regression

``` r
mlFit <- train(shares~timedelta+weekday+num_self_hrefs+num_imgs+num_videos, data=train, method="lm", preProcess=c("center", "scale"), trControl=trainControl(method="cv", number=10))
mlFit
```

    ## Linear Regression 
    ## 
    ## 4381 samples
    ##    5 predictor
    ## 
    ## Pre-processing: centered (10),
    ##  scaled (10) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 3943, 3945, 3942, 3943, 3944, 3942, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared    MAE     
    ##   13429.75  0.00575335  2985.822
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

    ##            LR  Boosted
    ## RMSE 6159.639 7377.526

``` r
best_model <- which.min(comp["RMSE",])
best_model
```

    ## LR 
    ##  1

LR has the minimum MSE which indicates the best fitting.
