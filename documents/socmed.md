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

    ## # A tibble: 2,323 x 62
    ##    url            timedelta n_tokens_title
    ##    <chr>              <dbl>          <dbl>
    ##  1 http://mashab~       731              8
    ##  2 http://mashab~       731              8
    ##  3 http://mashab~       731              9
    ##  4 http://mashab~       731             10
    ##  5 http://mashab~       729              9
    ##  6 http://mashab~       729              9
    ##  7 http://mashab~       729             10
    ##  8 http://mashab~       728              7
    ##  9 http://mashab~       727              8
    ## 10 http://mashab~       727              6
    ## # ... with 2,313 more rows, and 59 more
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

    ## [1] 1627   62

## Summarizations on train set

-   descriptive statistics on numeric variables:

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
    ##  n_non_stop_unique_tokens
    ##  Min.   :0.0000          
    ##  1st Qu.:0.6172          
    ##  Median :0.6830          
    ##  Mean   :0.6833          
    ##  3rd Qu.:0.7559          
    ##  Max.   :1.0000          
    ##    num_hrefs      num_self_hrefs  
    ##  Min.   :  0.00   Min.   : 0.000  
    ##  1st Qu.:  5.00   1st Qu.: 2.000  
    ##  Median :  8.00   Median : 3.000  
    ##  Mean   : 13.27   Mean   : 4.856  
    ##  3rd Qu.: 15.00   3rd Qu.: 5.000  
    ##  Max.   :110.00   Max.   :74.000  
    ##     num_imgs       num_videos    
    ##  Min.   : 0.00   Min.   : 0.000  
    ##  1st Qu.: 1.00   1st Qu.: 0.000  
    ##  Median : 1.00   Median : 0.000  
    ##  Mean   : 4.27   Mean   : 1.052  
    ##  3rd Qu.: 3.00   3rd Qu.: 1.000  
    ##  Max.   :62.00   Max.   :34.000  
    ##  average_token_length  num_keywords   
    ##  Min.   :0.000        Min.   : 1.000  
    ##  1st Qu.:4.496        1st Qu.: 5.000  
    ##  Median :4.657        Median : 7.000  
    ##  Mean   :4.644        Mean   : 6.615  
    ##  3rd Qu.:4.813        3rd Qu.: 8.000  
    ##  Max.   :5.774        Max.   :10.000  
    ##  self_reference_avg_sharess
    ##  Min.   :     0            
    ##  1st Qu.:  1506            
    ##  Median :  3300            
    ##  Mean   :  8963            
    ##  3rd Qu.:  7403            
    ##  Max.   :690400            
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

We can find the minimum, 25% percentile, mean, median, 75% percentile
and maximum values of each numeric variables from this chart.

``` r
sapply(train %>% select(timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_imgs, num_videos, average_token_length, num_keywords, self_reference_avg_sharess, self_reference_min_shares, self_reference_max_shares, global_rate_negative_words, global_rate_positive_words, global_sentiment_polarity, global_subjectivity, rate_negative_words, rate_positive_words, title_subjectivity, title_sentiment_polarity, abs_title_sentiment_polarity, abs_title_subjectivity), sd)
```

    ##                    timedelta 
    ##                 1.969391e+02 
    ##               n_tokens_title 
    ##                 2.094012e+00 
    ##             n_tokens_content 
    ##                 5.438069e+02 
    ##              n_unique_tokens 
    ##                 1.202043e-01 
    ##     n_non_stop_unique_tokens 
    ##                 1.137613e-01 
    ##                    num_hrefs 
    ##                 1.534742e+01 
    ##               num_self_hrefs 
    ##                 6.733202e+00 
    ##                     num_imgs 
    ##                 8.230572e+00 
    ##                   num_videos 
    ##                 3.055107e+00 
    ##         average_token_length 
    ##                 3.756737e-01 
    ##                 num_keywords 
    ##                 2.200461e+00 
    ##   self_reference_avg_sharess 
    ##                 2.716347e+04 
    ##    self_reference_min_shares 
    ##                 2.177762e+04 
    ##    self_reference_max_shares 
    ##                 5.200357e+04 
    ##   global_rate_negative_words 
    ##                 1.021347e-02 
    ##   global_rate_positive_words 
    ##                 1.684749e-02 
    ##    global_sentiment_polarity 
    ##                 9.352095e-02 
    ##          global_subjectivity 
    ##                 9.099611e-02 
    ##          rate_negative_words 
    ##                 1.342739e-01 
    ##          rate_positive_words 
    ##                 1.420393e-01 
    ##           title_subjectivity 
    ##                 3.178333e-01 
    ##     title_sentiment_polarity 
    ##                 2.677698e-01 
    ## abs_title_sentiment_polarity 
    ##                 2.381791e-01 
    ##       abs_title_subjectivity 
    ##                 1.849411e-01

From here we can compare standard deviation between numeric variables.

-   Correlation between numeric predictors

``` r
Correlation <- cor(train %>% select(timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_imgs, num_videos, average_token_length, num_keywords, self_reference_avg_sharess, self_reference_min_shares, self_reference_max_shares, global_rate_negative_words, global_rate_positive_words, global_sentiment_polarity, global_subjectivity, rate_negative_words, rate_positive_words, title_subjectivity, title_sentiment_polarity, abs_title_sentiment_polarity, abs_title_subjectivity))
corrplot(Correlation, type="upper", tl.pos="lt", cl.cex=0.8)
```

![](/documents/socmed_files/figure-gfm/unnamed-chunk-6-1.png)

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
    ## 1 Friday     227   213  57000 4226.   2200
    ## 2 Monday     240   200  57600 4079.   2550
    ## 3 Saturday   124   217  34500 3567.   2300
    ## 4 Sunday      92   636  54100 5273.   2750
    ## 5 Thursday   324   165  26900 3227.   2100
    ## 6 Tuesday    318   238 122800 3594.   1900
    ## 7 Wednesd~   302   398  59000 3486.   2000

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

![](/documents/socmed_files/figure-gfm/unnamed-chunk-8-1.png)

-   Scatter plot

We want to check the relationship between response variable `share` and
other predictors through scatter plot. Linear or non-linear? Positive or
negative?

``` r
g <- ggplot(train, aes(x=num_self_hrefs, y=shares, col=weekday) )
g + geom_point()
```

![](/documents/socmed_files/figure-gfm/unnamed-chunk-9-1.png)

``` r
g <- ggplot(train, aes(x=num_imgs, y=shares, col=weekday) )
g + geom_point()
```

![](/documents/socmed_files/figure-gfm/unnamed-chunk-10-1.png)

``` r
g <- ggplot(train, aes(x=rate_positive_words, y=shares, col=weekday) )
g + geom_point()
```

![](/documents/socmed_files/figure-gfm/unnamed-chunk-11-1.png)

## Modeling

### Linear Regression

``` r
mlFit <- train(shares~timedelta+weekday+num_self_hrefs+num_imgs+num_videos+rate_positive_words, data=train, method="lm", preProcess=c("center", "scale"), trControl=trainControl(method="cv", number=10))
mlFit
```

    ## Linear Regression 
    ## 
    ## 1627 samples
    ##    6 predictor
    ## 
    ## Pre-processing: centered (11), scaled (11) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1465, 1464, 1465, 1463, 1464, 1464, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared     MAE     
    ##   5856.472  0.005296394  2839.669
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
    ## RMSE 3938.956 3928.958

``` r
best_model <- which.min(comp["RMSE",])
best_model
```

    ## Boosted 
    ##       2

Boosted has the minimum MSE which indicates the best fitting.
