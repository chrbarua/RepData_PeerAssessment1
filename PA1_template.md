---
title: "Reproducible Research P1"
author: "Christopher Barua"
date: "`r Sys.Date()`"
output: html_document
---

## Loading and preprocessing the data

```{r, echo = TRUE}
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```{r, echo = TRUE} 
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
cmean <- mean(steps_by_day$steps)
cmedian <- median(steps_by_day$steps)
```
<img src="plot1.png"

* The total number of steps taken per day was `steps_by_day`.
* The mean and median of the total number of steps that were taken was `r cmean` and `r cmedian` respectively.

## What is the average daily activity pattern?

```{r, echo = TRUE}
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
```
<img src="plot2.png"

* The average steps for each interval for all days was `steps_by_interval`.
* The interval with the maximum number of steps was `r max_interval`.

## Imputing missing values

```{r, echo = TRUE}
missing <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)
cmean_i <- mean(steps_by_day_i$steps)
cmedian_i <- median(steps_by_day_i$steps)
mean_diff <- cmean_i - cmean
median_diff <- cmedian_i - cmedian
total_diff <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)
```
<img src="plot3.png"

* The total number of missing values in the data set is `r missing`.
* The mean and median of the total number of steps taken per day, with missing data imputed, is `r cmean_i` and `r cmedian_i` respectively.
* These values do differ from the original data. The mean has a difference of `r mean_diff`, and the median has a difference of `r median_diff`.
* The impact of imputing the missing data on the total number of daily steps resulted in the difference `r total_diff`.

## Are there differences in activity patterns between weekdays and weekends?

``` {r, echo = TRUE}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)
library(lattice)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```
<img src="plot4.png"
