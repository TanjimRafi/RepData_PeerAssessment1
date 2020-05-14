---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
setwd("F:/Data Science/Johns Hopkins University(Coursera)/5 . Reproducible Research/data/RepData_PeerAssessment1-master")
data <- read.csv(file = "activity.csv")
names(data)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(data)
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


## What is mean total number of steps taken per day?

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data$date <- as.Date(data$date , "%Y-%m-%d")
dates <- unique(data$date)
date_diff <- length(dates)

steps <- data$steps

total_steps_per_day <- data %>% 
    group_by(date) %>% 
    summarise(sum.steps = sum(steps , na.rm=TRUE))
avg_steps_per_day <- sum(steps, na.rm = TRUE)/date_diff
median_steps_per_day <- median(total_steps_per_day$sum.steps)

hist(total_steps_per_day$sum.steps       
     , main = "Histogram of Steps/Day"
     , xlab = "Steps/Day"
     , breaks = 10)
abline(v = avg_steps_per_day 
       , lwd = 3 
       , col = "red")
abline(v = median_steps_per_day
       , lwd = 3 
       , col = "blue")
legend("topright" 
       , legend = c("Mean steps/day","Median steps/day") 
       , col = c("red" , "blue")
       , lty = 1
       , lwd = 3
       , bty = "n"
       , cex = 0.8)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
The Mean number of steps taken per day is 9354.23 which is less than 10000
And the Median number of steps taken per day is 10395 which is more than 10000.

## What is the average daily activity pattern?

```r
data1 <- na.omit(data)
data2 <- data1 %>% group_by(interval) %>% summarise(mean.steps = mean(steps))
plot(x = data2$interval 
     , y = data2$mean.steps 
     , type = "l"
     , main = "The average daily activity pattern"
     , xlab = "5 minitue time interval"
     , ylab = "No. of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
data2$interval[which.max(data2$mean.steps)]
```

```
## [1] 835
```
The 5-minute interval 835, on average across all the days in the dataset, contains the maximum number of steps.

## Imputing missing values

```r
missing_data <- sum(is.na(data))

newData <- data

intervals <- length(newData$interval)/length(dates)
newData$steps[which(is.na(newData$steps))] <- avg_steps_per_day/intervals
head(newData)
```

```
##      steps       date interval
## 1 32.47996 2012-10-01        0
## 2 32.47996 2012-10-01        5
## 3 32.47996 2012-10-01       10
## 4 32.47996 2012-10-01       15
## 5 32.47996 2012-10-01       20
## 6 32.47996 2012-10-01       25
```

```r
library(dplyr)

data$date <- as.Date(newData$date , "%Y-%m-%d")
dates <- unique(newData$date)
date_diff <- length(dates)

steps <- newData$steps

total_steps_per_day <- newData %>% 
    group_by(date) %>% 
    summarise(sum.steps = sum(steps , na.rm=TRUE))
avg_steps_per_day <- sum(steps, na.rm = TRUE)/date_diff
median_steps_per_day <- median(total_steps_per_day$sum.steps)

hist(total_steps_per_day$sum.steps       
     , main = "Histogram of Steps/Day"
     , xlab = "Steps/Day"
     , breaks = 10)
abline(v = avg_steps_per_day 
       , lwd = 3 
       , col = "red")
abline(v = median_steps_per_day
       , lwd = 3 
       , col = "blue")
legend("topright" 
       , legend = c("Mean steps/day","Median steps/day") 
       , col = c("red" , "blue")
       , lty = 1
       , lwd = 3
       , bty = "n"
       , cex = 0.8)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
### Number of missing values

There are 2304 missing values(NA) in the data set . 

### Mean and median after imputing the NA's

#### Before imputing
The Mean number of steps taken per day is 9354.23 which is less than 10000
And the Median number of steps taken per day is 10395 which is more than 10000.

#### After imputing
The Mean number of steps taken per day is 10763.
And the Median number of steps taken per day is 10741.9.

So after imputing the NA's in the activity data set we can increase in both Mean and Median steps taken per day.

## Are there differences in activity patterns between weekdays and weekends?

```r
newData$date <- as.Date(newData$date , "%Y-%m-%d")
newData$days <- ifelse(weekdays(newData$date) %in% c("Saturday","Sunday"), "weekday", "weekend")

par(mfrow = c(2,1))

weekday <- filter(newData , days == "weekday")
weekday1 <- weekday %>% group_by(interval) %>% summarise(mean.steps = mean(steps))
plot(x = weekday1$interval
     , y = weekday1$mean.steps
     , type = "l"
     , xlab = "Intervals"
     , ylab = "Average Steps"
     , main = "Weekday")

weekend <- filter(newData , days == "weekend")
weekend1 <- weekend %>% group_by(interval) %>% summarise(mean.steps = mean(steps))
plot(x = weekend1$interval
     , y = weekend1$mean.steps
     , type = "l"     
     , xlab = "Intervals"
     , ylab = "Average Steps"
     , main = "Weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
