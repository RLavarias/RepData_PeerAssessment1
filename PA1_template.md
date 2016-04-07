Reproducible Research: FitBit Data

Author: Rommel Lavarias
================================================================================================================

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

Load the data

``` {r}
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

Aggregate the data of the steps per day, then make a histogram of the data:
``` {r}
steps_date <- aggregate(steps ~ date, data = activity, FUN = sum)
hist(steps_date$steps, main="Total Steps Per Day", xlab = "Steps", ylab = "Frequency")
```

![](PA1_template_files/figure-html/Repro1.png)

Then calculate the mean and the median:

``` {r}
mean(steps_date$steps)

median(steps_date$steps)

```

## What is the average daily activity pattern?

First we'll calculate the average steps for all days, then we'll plot the Average Number of Steps Per Day by Interval

``` {r}
steps_interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(steps_interval$interval, steps_interval$steps, type="l", xlab="Interval", ylab="Number of Steps", main="Average Number of Steps per 5 Minute Interval")
```

![](PA1_template_files/figure-html/Repro2.png)

To determine which 5-minute interval, on average across all the days contains the maximum number of steps:

``` {r}
steps_interval$interval[which.max(steps_interval$steps)]
##[1] 835
```

## Imputing missing values

First calculate the total number of missing values:

``` {r}
sum(is.na(activity))

```

The strategy I will use to fill in the missing data from the data sets is using the means of the 5 minute intervals 

``` {r}
activity <- merge(activity, steps_interval, by = "interval", suffixes = c("",".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]
```

Make a histogram of the data with the total number of steps taken per day adjusted with the missing data:

``` {r}
steps_date <- aggregate(steps ~ date, data = activity, FUN = sum)
hist(steps_date$steps, main = "Total Steps Each Day", xlab = "Frequency", ylab = "Number of Steps")
```

![](PA1_template_files/figure-html/Repro3.png)

Obtain the mean and the median of the total steps per day:

``` {r}
mean(steps_date$steps)


median(steps_date$steps)

```

Showing the impact of the missing data is minimal

## Are there differences in activity patterns between weekdays and weekends?

First I'll create a factor variable in the data set with two levels, one for Weekday and one for Weekend:

``` {r}
daytype <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```

Then I will plot the data to show the difference in activty between Weekdays and Weekends:

``` {r}
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
  steps_type <- aggregate(steps ~ interval, data = activity, subset = activity$daytype == type, FUN = mean)
  plot(steps_type, type = "l", main = type)
}
```
!(PA1_template_files/figure-html/Repro4.png)
