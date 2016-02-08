---
title: "Reproducible Research: Peer Assessment 1"
author: "Baiaman Urmatbek"
output: 
  html_document:
  keep_md: true
---


## Loading and preprocessing the data
```{r}
activity.data <- read.csv(unzip("activity.zip"), header = TRUE)


```

## Histogram of the total number of steps taken each day

```{r}

steps.per.day<- aggregate(steps ~ date, activity.data, sum)
hist(steps.per.day$steps, col = "red", xlab = "number of steps", main = "Number of Steps Taken per Day - October and November, 2012")


```


## Mean and median number of steps taken each day

```{r}
mean(steps.per.day$steps)
median(steps.per.day$steps)
```


## What is the average daily activity pattern?
```{r}
steps.per.day$date <- as.Date(steps.per.day$date)
steps.per.day$steps <- as.numeric(steps.per.day$steps)

plot(steps.per.day$date, steps.per.day$steps, type="l", xlab = "Date", ylab = "Number of steps", main = "Daily Activity Pattern - October and November, 2012", col = "red")

```


## Imputing missing values

* I will replace NAs with the mean of daily steps

```{r}

activity.data$steps[is.na(activity.data$steps)] <- mean(activity.data$steps[!is.na(activity.data$steps)])

steps.per.day.without.na<- aggregate(steps ~ date, activity.data, sum)
par(mfrow = c(1, 2))

hist(steps.per.day.without.na$steps, col = "blue", xlab = "number of steps", main = "Without NAs")

hist(steps.per.day$steps, col = "red", xlab = "number of steps", main = "With NAs")

```

## The 5-minute interval that, on average, contains the maximum number of steps

```{r}

which.max(steps.per.day$steps)

```



## Are there differences in activity patterns between weekdays and weekends?

```{r}

#steps.per.day.without.na$weekdays <- weekdays(steps.per.day.without.na$date)
steps.per.day.without.na$weekdayN <- as.POSIXlt(steps.per.day.without.na$date)$wday
weekday.mean.steps<- aggregate(steps ~ weekdayN, steps.per.day.without.na, mean)
plot(weekday.mean.steps$weekdayN, weekday.mean.steps$steps, type = "l", xlab = "Weekdays", ylab = "Number of Steps", main = "Average Number of Steps")
legend("bottomright", c("0 - Sunday", "1 - Monday", "2 - Tuesday", "3 - Wednesday", "4 - Thursday", "5 - Friday", "6 - Saturday"), lty=1, lwd=2.5, col=c("black", "red", "blue","black", "red", "blue", "black"))

```

```{r}

steps.weekdays <- mean(weekday.mean.steps[(2:6),2])
steps.weekends <- mean(weekday.mean.steps[-(2:6),2])

average.steps <- cbind(steps.weekdays, steps.weekends)
names(average.steps) <- c("weekday", "weekdend")
barplot(average.steps, col = c("red", "blue"), main = "Differences Between Weekdays and Weekends", ylab = "Number of Steps") 

```

##Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r}



activity.data$weekdayN <- as.POSIXlt(activity.data$date)$wday
activity.data$date <- as.Date(activity.data$date)
activity.data$weekdays <- weekdays(activity.data$date)
activity.data.weekdays <- subset(activity.data, activity.data$weekdays == c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
activity.data.weekends <- subset(activity.data, activity.data$weekdays == "Saturday" | activity.data$weekdays == "Sunday")

par(mfrow = c(1, 2))

plot(activity.data.weekends$interval, activity.data.weekends$steps, type = "l", main="Average Steps on Weekends", xlab="Interval", ylab="Steps")

plot(activity.data.weekdays$interval, activity.data.weekdays$steps, type = "l", main="Average Steps on Weekdays",xlab="Interval", ylab="Steps")

```


