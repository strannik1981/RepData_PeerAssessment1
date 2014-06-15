setwd("./repdata-003/RepData_PeerAssessment1")
raw.data <- read.csv(
    unz("activity.zip","activity.csv"),
    colClasses=c("integer","Date","integer"))

date.sum <- aggregate(. ~ date, data=raw.data, FUN=sum)
date.sum$interval <- NULL
date.mean <- aggregate(. ~ date, data=raw.data, FUN=mean)
date.mean$interval <- NULL
hist(date.sum$steps, xlab="Number of steps", 
     main="Histogram of steps taken per day")
mean(date.sum$steps)
median(date.sum$steps)

interval.mean <- aggregate(. ~ interval, data=raw.data, FUN=mean)
interval.mean$date <- NULL
plot(interval.mean, type="l")
interval.mean[interval.mean$steps == max(interval.mean$steps), "interval"]

index <- is.na(raw.data$steps)
sum(index)

filled.data <- raw.data
filled.data$steps[index] <- as.numeric(
    apply(raw.data[index,], MARGIN = 1, 
          FUN = function ( x ) 
              { interval.mean[interval.mean$interval == as.integer(x["interval"]), "steps"] }))
sum(is.na(filled.data$steps))

filled.sum <- aggregate(. ~ date, data=filled.data, FUN=sum)
filled.sum$interval <- NULL
hist(filled.sum$steps, xlab="Number of steps", 
     main="Histogram of steps taken per day (interpolated data)")
mean(filled.sum$steps)
median(filled.sum$steps)

filled.data$weekday <- factor(
    weekdays(filled.data$date) %in% c("Saturday","Sunday"), 
    labels = c("Weekday","Weekend"))

weekday.interval.mean <- aggregate(. ~ interval + weekday, data=filled.data, FUN=mean)
weekday.interval.mean$date <- NULL
library(lattice)
xyplot(steps ~ interval | weekday, data=weekday.interval.mean, type="l", layout=c(1,2), xlab="Interval", ylab="Number of steps")
