---
title: "Reproducible Research Course Project 1"
output: html_document
---



## Loading and Preprocessing the data

Load data from the local directory into the object "activity", and show the summary of its statistics.


```r
activity <- read.csv('./data/activity.csv',stringsAsFactors = FALSE)
summary(activity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

Transform the variable, date, into POSIXct format using lubridate package.


```r
library(lubridate)
activity$date <- ymd(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is the mean total number of steps?

The total number of steps taken every day is listed as follows,


```r
require(dplyr)
```


```r
avg_steps <- group_by(activity,date) %>% summarize(totalSteps = sum(steps,na.rm=TRUE), na =
                                                       sum(is.na(steps)))
avg_steps
```

```
## Source: local data frame [61 x 3]
## 
##          date totalSteps    na
##        (date)      (int) (int)
## 1  2012-10-01          0   288
## 2  2012-10-02        126     0
## 3  2012-10-03      11352     0
## 4  2012-10-04      12116     0
## 5  2012-10-05      13294     0
## 6  2012-10-06      15420     0
## 7  2012-10-07      11015     0
## 8  2012-10-08          0   288
## 9  2012-10-09      12811     0
## 10 2012-10-10       9900     0
## ..        ...        ...   ...
```

We visualize the total number of steps taken each day using a histogram,


```r
hist(avg_steps$totalSteps, breaks =20, col='red', main = 'Total number of steps taken per day',xlab = 'Total number of steps taken per day')
```

![plot of chunk histogram](figure/histogram-1.png)

Furthermore, we are able to derive the mean and median of total steps taken per day.


```r
mean_steps <- mean(avg_steps$totalSteps, na.rm = TRUE)
median_steps <- median(avg_steps$totalSteps, na.rm =TRUE)
```

The mean and median of total steps per day are 9354 and 10395 respectively.

## What is the average daily activity pattern?

In this section, we look at the daily activity pattern using 5-minute interval averaged across all days. First, we group the dataset by 5-minute interval, and present the average steps in a table,


```r
daily_pattern <- group_by(activity, interval) %>% summarise(avg_interval = mean(steps,na.rm= TRUE))
daily_pattern
```

```
## Source: local data frame [288 x 2]
## 
##    interval avg_interval
##       (int)        (dbl)
## 1         0    1.7169811
## 2         5    0.3396226
## 3        10    0.1320755
## 4        15    0.1509434
## 5        20    0.0754717
## 6        25    2.0943396
## 7        30    0.5283019
## 8        35    0.8679245
## 9        40    0.0000000
## 10       45    1.4716981
## ..      ...          ...
```

As a result, we are able to visualize the table through a time series plot,


```r
plot(daily_pattern$interval, daily_pattern$avg_interval, type='l',col='blue', xlab = '5-minute interval', ylab = 'Average 5-minute steps across all days', main = 'Average daily activity pattern')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Next, we see which maximum 5-minute interval contains the maximum number of steps averaged across all days, and the corresponding figure.


```r
max_interval <- filter(daily_pattern, avg_interval == max(avg_interval))
max_interval
```

```
## Source: local data frame [1 x 2]
## 
##   interval avg_interval
##      (int)        (dbl)
## 1      835     206.1698
```

In summary, interval "835" contains the maximum number of steps (206).

## Inputting missing values

In this section, we will attempt to deal with missing values. First, we look at how many missing values of steps there are in the original dataset.


```r
num_na <- summarize(avg_steps,sum(na))
```

There are 2304 missing values (i.e., total number of rows with NAs) in total in the dataset.

Next, we devise a strategy to fill in these missing values. As there are a large amount of missing values, which concentrate on eight specific days, we choose to utilize the average number of 5-minute steps. In doing so, the resulting dataset would be more informative of the daily activity pattern shown in the last section. We take a bridf look at the modified dataset with an extra variable of steps with missing values filled in.


```r
steps_modified <- numeric(nrow(activity))
for (i in 1:nrow(activity)) {
    if (is.na(activity[i,'steps'])) {
        steps_modified[i] <- filter(daily_pattern, interval == activity[i,'interval']) %>% select(avg_interval)
    }
    else {
        steps_modified[i] <- activity[i,'steps']
    }
}

steps_modified <- as.numeric(steps_modified)
activity_modified <- mutate(activity, steps_no_NA = steps_modified)
head(activity_modified)
```

```
##   steps       date interval steps_no_NA
## 1    NA 2012-10-01        0   1.7169811
## 2    NA 2012-10-01        5   0.3396226
## 3    NA 2012-10-01       10   0.1320755
## 4    NA 2012-10-01       15   0.1509434
## 5    NA 2012-10-01       20   0.0754717
## 6    NA 2012-10-01       25   2.0943396
```

```r
str(activity_modified)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps      : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date       : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval   : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps_no_NA: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

As previously, we draw a histogram of total number of steps taken each day for the modified dataset.


```r
avg_steps_modified <- group_by(activity_modified,date) %>% summarize(totalSteps = sum(steps_no_NA,na.rm=TRUE), na = sum(is.na(steps)))
hist(avg_steps_modified$totalSteps,breaks=20,col='magenta', main = 'Total number of steps taken each day', xlab = 'Total number of steps taken each day')
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

Furthermore, we are able to derive the mean and median of total steps taken per day for the modified model.


```r
mean_steps_modified <- mean(avg_steps_modified$totalSteps, na.rm = TRUE)
median_steps_modified <- median(avg_steps_modified$totalSteps, na.rm =TRUE)
```

The mean and median of total steps per day for the modified model are 10766 and 10766 respectively. Compared to the outcome obtained when ignoring missing values, both figures are a bit larger, which makes the analysis more reasonable.

## Are there differences in activity patterns between weekdays and weekends?

In the following, we add a factor variable to the modified dataset in order to determine whether a day is a weekday or weekend.


```r
is_weekdays <- rep(0,nrow(activity_modified))
for (i in 1:nrow(activity_modified)) {
    if (weekdays.Date(activity_modified[i,'date'],abbreviate = TRUE) %in% c('Mon','Tue','Wed','Thu','Fri')) {
        is_weekdays[i] <- 'weekday' 
    }
    else {
        is_weekdays[i] <- 'weekend'
    }
}
is_weekdays <- as.factor(is_weekdays)
table(is_weekdays)
```

```
## is_weekdays
## weekday weekend 
##   12960    4608
```

```r
activity_modified <- mutate(activity_modified, week = is_weekdays)
str(activity_modified)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps      : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date       : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval   : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps_no_NA: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ week       : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

Last, we observe the daily activity pattern for weekdays and weekends respectively, in time series plots. 


```r
daily_pattern_week <- group_by(activity_modified,week,interval) %>% summarize(avg_interval=mean(steps_no_NA))
library(ggplot2)
qplot(interval,avg_interval,data = daily_pattern_week,geom = 'line',facets = week ~ ., main = 'Daily activity pattern for weekdays and weekends', xlab = '5-minute interval')
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
