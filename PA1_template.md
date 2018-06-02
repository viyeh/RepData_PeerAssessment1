---
title: "PA1_template"
output: 
  html_document: 
    keep_md: yes
keep_md: true
---





```r
##Load the data
activity = read.csv("C:/Users/viyeh/Downloads/repdata%2Fdata%2Factivity/activity.csv")

##Process/transform the data (if necessary) into a format suitable for your analysis
totalSteps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```


```r
## What is mean total number of steps taken per day? Make a histogram of the total number of steps taken each day
hist(totalSteps$steps,col="blue",main="Total Steps Taken Per Day",xlab="Total Steps Taken Per Day",cex.axis=1,cex.lab = 1)
```

![](PA1_template_files/figure-html/TotalStepsHistogram-1.png)<!-- -->


```r
##Calculate and report the mean and median total number of steps taken per day
mean_steps <- mean(totalSteps$steps)
median_steps <- median(totalSteps$steps)
```


```r
## What is the average daily activity pattern? Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

steps_interval <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
plot(steps ~ interval, data = steps_interval, type = "l", xlab = "5-minute Time Intervals", ylab = "Average number of steps taken (all Days)", main = "Average Number of Steps Taken at 5 Minute Intervals",  col = "blue")
```

![](PA1_template_files/figure-html/AvgDailyActivity-1.png)<!-- -->


```r
## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
  
maxStepInterval <- steps_interval[which.max(steps_interval$steps),"interval"]
  ##835 interval contains the maximum number of steps
```


```r
##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
missing_rows <- sum(!complete.cases(activity))
  ##The total number of missing rows is 2304
```


```r
##Devise a strategy for filling in all of the missing values in the dataset. 
##The strategy does not need to be sophisticated. 
##For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

## This function returns the mean steps for a given interval
getMeanStepsPerInterval <- function(interval){
  steps_interval[steps_interval$interval==interval,"steps"]
}

##Create a new dataset that is equal to the original dataset but with the missing data filled in.

complete.activity <- activity

## Filling the missing values with the mean for that 5-minute interval
flag = 0
for (i in 1:nrow(complete.activity)) {
  if (is.na(complete.activity[i,"steps"])) {
    complete.activity[i,"steps"] <- getMeanStepsPerInterval(complete.activity[i,"interval"])
    flag = flag + 1
  }
}
  ##Total of 2304 missing values were filled.
```


```r
total.steps.per.days <- aggregate(steps ~ date, data = complete.activity, sum)
hist(total.steps.per.days$steps, col = "blue", xlab = "Total Number of Steps", 
     ylab = "Frequency", main = "Histogram of Total Number of Steps taken each Day")
```

![](PA1_template_files/figure-html/TotalSteps-1.png)<!-- -->


```r
## Calculate and report the mean and median total number of steps taken per day.
showMean <- mean(total.steps.per.days$steps)
showMedian <- median(total.steps.per.days$steps)
  ##Mean total number of steps taken per day is 1.0766 × 104
  ##Median total number of steps taken per day is 1.0766 × 104
```


```r
##Do these values differ from the estimates from the first part of the assignment?
  ##The mean value is the same as the value before inuting missing data, but the median value has changed.
## What is the impact of inputing missing data on the estimates of the total daily number of steps?
  ##The mean value is the same as the value before inputing missing data since the mean value has been used for that particular 5-min interval. The median value is different, since the median index is now being changed after imputing missing values.

##Are there differences in activity patterns between weekdays and weekends?
  
##Create a new factor variable in the dataset with two levels - "weekday"" and "weekend"" indicating whether a given date is a weekday or weekend day.

complete.activity$day <- ifelse(as.POSIXlt(as.Date(complete.activity$date))$wday%%6 == 
0, "weekend", "weekday")
complete.activity$day <- factor(complete.activity$day, levels = c("weekday", "weekend"))
```


```r
##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

steps.interval= aggregate(steps ~ interval + day, complete.activity, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = steps.interval, aspect = 1/2, 
       type = "l")
```

![](PA1_template_files/figure-html/WeekdayWeekendPlot-1.png)<!-- -->
s
