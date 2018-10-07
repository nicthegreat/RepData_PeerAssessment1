---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Introduction and Loading data

This is an R Markdown document has been created for Coursera's Reproducible Research Project 1 assignment.


```r
activity <- read.csv('activity.csv')
```

## Basic Summary Stats

Here is the output of some basic summary stats of the dataset "activity"


```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```


```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## Question 1: What is mean total number of steps taken per day?

Calculate the total number of steps taken per day


```r
aggregate(steps~date, activity, sum, na.action = na.omit)
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

Make a histogram of the total number of steps taken each day


```r
group <- aggregate(steps~date, activity, sum, na.action = na.omit)

hist(group$steps, main="Histogram of total number of steps per day", 
     xlab="Total number of steps per day")
```

![](PA1_template_files/figure-html/stepsperdayh-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day


```r
meanSteps <- mean(group$steps)
sprintf("Mean number of steps per day is %s", meanSteps)
```

```
## [1] "Mean number of steps per day is 10766.1886792453"
```

```r
medianSteps <- median(group$steps)
sprintf("Median number of steps per day is %s", medianSteps)
```

```
## [1] "Median number of steps per day is 10765"
```

## Question 2: What is the average daily activity pattern?

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
interval <- aggregate(steps~interval, activity, sum, na.action = na.omit)

plot(interval$interval, interval$steps, type='l', 
     main="Average number of steps averaged across over all days", xlab="Interval", 
     ylab="Average number of steps")
```

![](PA1_template_files/figure-html/stepsperinterval-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxSteps <- which.max(interval$steps)

maxInterval <- interval[maxSteps,]$interval
sprintf("The 5-min interval which contains the largest number of steps is at minute %s", maxInterval)
```

```
## [1] "The 5-min interval which contains the largest number of steps is at minute 835"
```

## Question 3: Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
naCount <- sum(is.na(activity$steps))

sprintf("The total number of NAs in the dataset is %s", naCount)
```

```
## [1] "The total number of NAs in the dataset is 2304"
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_new <- activity

activity_new$steps[is.na(activity_new$steps)] <- median(activity_new$steps, na.rm=TRUE)
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
group_new <- aggregate(steps~date, activity_new, sum, na.action = na.omit)

hist(group_new$steps, main="Histogram of total number of steps per day", 
     xlab="Total number of steps per day")
```

![](PA1_template_files/figure-html/imputation-1.png)<!-- -->

```r
meanSteps_new <- mean(group_new$steps)
sprintf("Mean number of steps per day of the imputed dataset is %s", meanSteps_new)
```

```
## [1] "Mean number of steps per day of the imputed dataset is 9354.22950819672"
```

```r
medianSteps_new <- median(group_new$steps)
sprintf("Median number of steps per day of the imputed dataset is %s", medianSteps_new)
```

```
## [1] "Median number of steps per day of the imputed dataset is 10395"
```


## Question 4: Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity$weekday = weekdays(as.Date(activity$date))
activity$weekday.type <- ifelse(activity$weekday == "Saturday" | activity$weekday == 
    "Sunday", "Weekend", "Weekday")
activity$weekday.type <- factor(activity$weekday.type)
```


Make a panel plot containing a time series plot type = 'l' of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avgStep <- aggregate(steps ~ interval + weekday.type, activity, mean, na.action = na.omit)
library(lattice)
xyplot(steps ~ interval | weekday.type, avgStep, type = "l", lwd = 2,
       layout = c(1, 2), 
       xlab = "5-minute interval", 
       ylab = "Average number of steps",
       main = "Average Number of Steps Taken (across all weekdays or weekends)")
```

![](PA1_template_files/figure-html/panelplot-1.png)<!-- -->
