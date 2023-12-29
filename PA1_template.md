---
title: "PA1_template"
author: "Stavros Tzanidakis"
date: "2023-12-29"
output:
    
  html_document:
     keep_md: true
    
---


## set working directory

```r
setwd("~/Coursera/Reproducible Research/Week_2/RepData_PeerAssessment1")
```


## 1. Loading and preprocessing the data

```r
activity <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric","character","integer"))
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activity$date <- as.Date(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
## 2. What is mean total number of steps taken per day?

```r
stepsperday <- aggregate(steps~date,activity,sum)
```
##### 2.1. Make a histogram of the total number of steps taken each day

```r
hist(stepsperday$steps, main = paste("Total steps per day"), col="red", xlab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
##### 2.2 Calculate and report the mean and median total number of steps taken per day

```r
Mean <- mean(stepsperday$steps)
Median <- median(stepsperday$steps)
sprintf("MEAN of steps taken each day = %.3f", Mean)
```

```
## [1] "MEAN of steps taken each day = 10766.189"
```

```r
sprintf("MEDIAN of steps taken each day = %.3f", Median)
```

```
## [1] "MEDIAN of steps taken each day = 10765.000"
```
## 3. What is the average daily activity pattern?
#### 3.1 ## Time series plot of the average number of steps taken

```r
stepsperinterval <- aggregate(steps ~ interval, activity, mean)
plot(stepsperinterval$interval,stepsperinterval$steps, type="l", xlab="5-minute interval", ylab="Number of steps",main="Average number of steps per day by interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
#### 3.2 The 5-minute interval that, on average, contains the maximum number of steps

```r
maxinterval <- stepsperinterval[which.max(stepsperinterval$steps),1]
sprintf("MAXIMUM number of steps in 5-minute interval = %.0f", maxinterval)
```

```
## [1] "MAXIMUM number of steps in 5-minute interval = 835"
```
## 4. Imputing missing values
#### 4.1 Calculate and report the total number of missing values in the dataset

```r
nadata <- sum(!complete.cases(activity))
sprintf("MISSING data = %.0f", nadata)
```

```
## [1] "MISSING data = 2304"
```
#### 4.2 Strategy for filling in all of the missing values in the dataset. Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
imputedata <- transform(activity, steps = ifelse(is.na(activity$steps), stepsperinterval$steps[match(activity$interval, stepsperinterval$interval)], activity$steps))
```
#### 4.3 Make a histogram of the total number of steps taken each day.Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 

```r
imputedstepsperday <- aggregate(steps ~ date, imputedata, sum)
par(mfrow=c(2,1))
hist(imputedstepsperday$steps, main = paste("Total steps per day with imputed data"), col="green", xlab="Number of steps")
hist(stepsperday$steps, main = paste("Total steps per day with non-imputed data"), col="red", xlab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
#### 4.3 Calculate and report the mean number of steps taken per day

```r
rmean <- mean(stepsperday$steps)
imputed_rmean <- mean(imputedstepsperday$steps)
sprintf("MEAN of steps taken each day = %.3f", rmean)
```

```
## [1] "MEAN of steps taken each day = 10766.189"
```

```r
sprintf("MEAN of steps taken each day with IMPUTED data = %.3f", imputed_rmean)
```

```
## [1] "MEAN of steps taken each day with IMPUTED data = 10766.189"
```

```r
sprintf("The difference is %.3f ", imputed_rmean-rmean)
```

```
## [1] "The difference is 0.000 "
```
#### 4.4 Calculate and report the median number of steps taken per day

```r
rmedian <- median(stepsperday$steps)
imputed_rmedian <- median(imputedstepsperday$steps)
sprintf("MEDIAN of steps taken each day = %.3f", rmedian)
```

```
## [1] "MEDIAN of steps taken each day = 10765.000"
```

```r
sprintf("MEDIAN of steps taken each day with IMPUTED data = %.3f", imputed_rmedian)
```

```
## [1] "MEDIAN of steps taken each day with IMPUTED data = 10766.189"
```

```r
sprintf("The difference is %.3f ", imputed_rmedian-rmedian)
```

```
## [1] "The difference is 1.189 "
```
#### 4.5 Calculate and report the total number of steps taken per day

```r
total <- sum(stepsperday$steps)
imputed_total <- sum(imputedstepsperday$steps)
sprintf("TOTAL of steps = %.3f", total)
```

```
## [1] "TOTAL of steps = 570608.000"
```

```r
sprintf("TOTAL of steps with IMPUTED data = %.3f", imputed_total)
```

```
## [1] "TOTAL of steps with IMPUTED data = 656737.509"
```

```r
sprintf("The difference is %.3f ", imputed_total-total)
```

```
## [1] "The difference is 86129.509 "
```

```r
## Including Plots
```
## 5 Are there differences in activity patterns between weekdays and weekends?

```r
weekenddays <- c("Saturday", "Sunday")
imputedata$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputedata$date)),weekenddays), "Weekend", "Weekday"))
imputedstepsperinterval <- aggregate(steps ~ interval + dow, imputedata, mean)

library(lattice)

xyplot(imputedstepsperinterval$steps ~ imputedstepsperinterval$interval|imputedstepsperinterval$dow, main="Average steps per day by interval",xlab="5-minute interval", ylab="Steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
