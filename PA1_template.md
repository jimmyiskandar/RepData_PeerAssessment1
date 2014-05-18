# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Load the data (i.e. read.csv())

```r
data <- read.csv("activity\\activity.csv")
```

Process/transform the data (if necessary) into a format suitable for your analysis

```r
data$date <- as.Date(data$date)
```


## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
Make a histogram of the total number of steps taken each day

```r
noNAData <- data[complete.cases(data), ]
stepsByDay <- sapply(split(noNAData, noNAData$date), function(x) {
    sum(x$steps)
})
barplot(stepsByDay, main = "Histogram of Total Number of Steps by Day", xlab = "Date", 
    ylab = "Total number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Calculate and report the mean and median total number of steps taken per day

```r
mean(stepsByDay)
```

```
## [1] 10766
```

```r
median(stepsByDay)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsByInterval <- sapply(split(data, data$interval), function(x) {
    mean(x$steps, na.rm = TRUE)
})
plot(as.numeric(names(stepsByInterval)), stepsByInterval, type = "l", main = "Average Number of Steps by Interval", 
    xlab = "Interval", ylab = "Average Number of Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# interval with max average step
maxStep <- max(stepsByInterval)
names(stepsByInterval)[which(stepsByInterval == maxStep)]
```

```
## [1] "835"
```


## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
# total number of missing steps
naidx <- which(is.na(data$steps))
length(naidx)
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
# strategy: replace missing steps with average steps for the particular
# interval
mi <- match(data$interval[naidx], as.numeric(names(stepsByInterval)))
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# create a new data with missing steps replaced with average steps for the
# particular interval
newData <- data
newData$steps[naidx] <- stepsByInterval[mi]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
newStepsByDay <- sapply(split(newData, newData$date), function(x) {
    sum(x$steps)
})
barplot(newStepsByDay, main = "Histogram of Total Number of Steps by Day", xlab = "Date", 
    ylab = "Total number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
mean(newStepsByDay)
```

```
## [1] 10766
```

```r
median(newStepsByDay)
```

```
## [1] 10766
```

Yes, the mean and median differ from the first part. There is negligible impact of imputing missing data on the estimates of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
newData$weekday <- weekdays(newData$date)
```


Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
weekendidx <- which(newData$weekday == "Saturday" | newData$weekday == "Sunday")
weekendData <- newData[weekendidx, ]
workdayData <- newData[-weekendidx, ]

par(mfcol = c(2, 1))
weekendStepsByInterval <- sapply(split(weekendData, weekendData$interval), function(x) {
    mean(x$steps, na.rm = TRUE)
})
plot(as.numeric(names(weekendStepsByInterval)), weekendStepsByInterval, type = "l", 
    main = "Weekend", xlab = "Interval", ylab = "Average Number of Steps")

workdayStepsByInterval <- sapply(split(workdayData, workdayData$interval), function(x) {
    mean(x$steps, na.rm = TRUE)
})
plot(as.numeric(names(workdayStepsByInterval)), workdayStepsByInterval, type = "l", 
    main = "Weekday", xlab = "Interval", ylab = "Average Number of Steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

