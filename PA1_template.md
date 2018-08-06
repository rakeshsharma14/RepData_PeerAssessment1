---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    keep_md: true
---


## Loading and preprocessing the data




```r
activity <- read.csv(file = "./activity/activity.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
```
### Summary of the data

```r
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

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### formatting of the date

```r
activity$date <- as.Date(as.character(activity$date), "%Y-%m-%d")
```
## What is mean total number of steps taken per day?

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
by_day <- aggregate(activity$steps ~ activity$date, FUN = sum)
names(by_day) <- c("day", "sum")
barplot(height = by_day$sum, names.arg = by_day$day, cex.name = 0.68, las = 3, col = "orange")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# calculate the mean and median
mean <- mean(by_day$sum)
median <- median(by_day$sum)
as.table(c(mean, median))
```

```
##        A        B 
## 10766.19 10765.00
```


## What is the average daily activity pattern?

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
average_by_interval <- activity %>% group_by(interval) %>% summarise(average = mean(steps, na.rm = TRUE))
plot(x = average_by_interval$interval, y = average_by_interval$average, type = "l", col = "red", xaxt = "n", xlab = "Intervals", ylab = "Average number of days")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
max_numb_steps_interval <- filter(average_by_interval,average == max(average))
```

## Imputing missing values

```r
na_number <- sum(is.na(activity$steps))
na_number
```

```
## [1] 2304
```

```r
percentage_na <- mean(is.na(activity$steps))
percentage_na
```

```
## [1] 0.1311475
```

```r
# mutatting by index
indexes <- which(is.na(activity$steps) == TRUE)
activity_without_na <- data.frame(activity)
head(activity_without_na)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
activity_without_na[indexes, ]$steps <- mean(activity$steps, na.rm = TRUE)

# checking if any step is still NA
any(is.na(activity_without_na$steps))
```

```
## [1] FALSE
```

```r
by_day <- aggregate(activity_without_na$steps ~ activity$date, FUN = sum)
names(by_day) <- c("day", "sum")
barplot(height = by_day$sum, names.arg = by_day$day, cex.name = 0.68, las = 3, col = "orange")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
# calculate the mean and median
mean <- mean(by_day$sum)
median <- median(by_day$sum)
as.table(c(mean, median))
```

```
##        A        B 
## 10766.19 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?

```r
activity_without_na$day_type <- ifelse(wday(activity_without_na$date, label = TRUE) %in% c("Sat", "Sun"), "weekend", "weekday")

library(ggplot2)
by_interval_day_type<- activity_without_na %>% group_by(interval, day_type) %>% summarise(average = mean(steps))
by_interval_day_type
```

```
## # A tibble: 576 x 3
## # Groups:   interval [?]
##    interval day_type  average
##       <int>    <chr>    <dbl>
##  1        0  weekday 7.006569
##  2        0  weekend 4.672825
##  3        5  weekday 5.384347
##  4        5  weekend 4.672825
##  5       10  weekday 5.139902
##  6       10  weekend 4.672825
##  7       15  weekday 5.162124
##  8       15  weekend 4.672825
##  9       20  weekday 5.073235
## 10       20  weekend 4.672825
## # ... with 566 more rows
```

```r
# qplot(interval, average, data = by_interval_day_type, geom = "line", facets = day_type)
par(mfrow= c(2, 1))
weekday <- subset(by_interval_day_type, day_type == "weekday")
weekend <- subset(by_interval_day_type, day_type == "weekend")
plot(weekday$interval, weekday$average, type = "l", xlab = "interval", ylab = "average", col = "blue", main = "weekday")
plot(weekend$interval, weekend$average, type = "l", xlab = "interval", ylab = "average", col = "blue", main = "weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


