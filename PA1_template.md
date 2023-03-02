---
title: "week2_courseproject1"
author:"supraja"
---


```r
knitr:: opts_chunk$set(echo = TRUE)
```

## Loading Libraries

```r
library(data.table)
library(dplyr)
library(ggplot2)
```

## Loading and Preprocessing the data

```r
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
```

### Q1] What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
steps_per_day <- activity %>% group_by(date) %>% summarise(total_steps = sum(steps, na.rm = TRUE))
```

2. Make a histogram of the total number of steps taken each day

```r
hist(steps_per_day$total_steps, main = "Total Steps", xlab = "Steps")
```

![plot of chunk unnamed-chunk-44](figure/unnamed-chunk-44-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps_per_day <- round(mean(steps_per_day$total_steps), digits = 0)
median_steps_per_day <- median(steps_per_day$total_steps)
```

The mean is 9354 and the median is `r `median_steps_per_day`

### Q2] What is the average daily activity pattern?
1. Make a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps_per_5mins <- activity %>% group_by(interval) %>% summarise(avg_steps = mean(steps, na.rm = TRUE))
plot(steps_per_5mins$interval, steps_per_5mins$avg_steps,type = "l",
     main = "Average Daily Activity", xlab = "5 min interval", ylab = "average steps")  
```

![plot of chunk unnamed-chunk-46](figure/unnamed-chunk-46-1.png)
<br>2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
print(paste("The 5 min interval which contains the maximum number of steps is ",steps_per_5mins$interval[which.max(steps_per_5mins$avg_steps)]))
```

```
## [1] "The 5 min interval which contains the maximum number of steps is  835"
```

## Q3] Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
print(paste("Total NA's = ", sum(is.na(activity$steps))))
```

```
## [1] "Total NA's =  2304"
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Imputing using mean value of 5 min intervals


```r
imputed_activity <- activity
for (i in 1:nrow(activity)){
        if(is.na(activity$steps[i])){
                imputed_activity$steps[i]<- steps_per_5mins$avg_steps[imputed_activity$interval[i] == steps_per_5mins$interval]
        }
}
```

3. Make a histogram of the total number of steps taken each day.

```r
steps_per_day <- imputed_activity %>% group_by(date) %>% summarise(total_steps = sum(steps, na.rm = TRUE))
hist(steps_per_day$total_steps, main = "Total Steps", xlab = "Steps")
```

![plot of chunk unnamed-chunk-50](figure/unnamed-chunk-50-1.png)
<br>mean and median total number of steps taken per day.

```r
imputed_mean <- round(mean(steps_per_day$total_steps), digits = 0)
imputed_median <- round(median(steps_per_day$total_steps), digits = 0)
print(paste("Mean = ", imputed_mean))
```

```
## [1] "Mean =  10766"
```

```r
print(paste("Median = ", imputed_median))
```

```
## [1] "Median =  10766"
```
Comparison

```r
print(paste("Before Imputing: Mean = ", mean_steps_per_day, " , Median = ", median_steps_per_day))
```

```
## [1] "Before Imputing: Mean =  9354  , Median =  10395"
```

```r
print(paste(" After Imputing: Mean = ", imputed_mean, " , Median = ", imputed_median))
```

```
## [1] " After Imputing: Mean =  10766  , Median =  10766"
```
Both mean and median number of steps increases after imputing NA values

### Q4] Are there differences in activity patterns between weekdays and weekends?

1] Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity_week <- imputed_activity
activity_week$date <- as.Date(imputed_activity$date)
activity_week$day <- ifelse(weekdays(imputed_activity$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activity_week$day <- as.factor(activity_week$day)
```


2] Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
activity_weekday <- activity_week[activity_week$day == "weekday",] %>% group_by(interval) %>% summarise(mean_steps = mean(steps))
activity_weekday$day <- "weekday"

activity_weekend <- activity_week[activity_week$day == "weekend",] %>% group_by(interval) %>% summarise(mean_steps = mean(steps))
activity_weekend$day <- "weekend"

merged_week <- rbind(activity_weekday, activity_weekend)
merged_week$day <- as.factor(merged_week$day)
g <- ggplot (merged_week, aes (interval, mean_steps))
g + geom_line() + facet_grid (day~.) + 
        theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14)) + 
        labs(y = "Number of Steps") + labs(x = "Interval") + 
        ggtitle("Average Steps: Weekday vs. Weekend") + 
        theme(plot.title = element_text(hjust = 0.5))
```

![plot of chunk unnamed-chunk-54](figure/unnamed-chunk-54-1.png)
On weekdays, more steps in the morning as compared to weekdays
On weekends, nearly consistent steps in the entire day
