# Reproducible Research: Peer Assessment 1

##Loading and preprocessing the data

1. Load the data


```r
activityData<-read.csv(file="activity/activity.csv", header=TRUE, sep=",")
```

##What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
stepsByDate <- aggregate(activityData$steps, by=list(date=activityData$date), FUN=sum, na.rm=TRUE)
```

2. Make a histogram of the total number of steps taken each day


```r
hist(stepsByDate$x,
    breaks=10,
    col = "red",
    main = "Histogram of the total number of steps taken each day",
    xlab = "Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(stepsByDate[["x"]])
```

```
## [1] 9354.23
```

```r
median(stepsByDate[["x"]])
```

```
## [1] 10395
```

##What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsByInterval <- aggregate(activityData$steps, by=list(interval=activityData$interval), FUN=sum,na.rm=TRUE)
with(stepsByInterval,plot(interval,x,type='l',xlab="Intervals",ylab="Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsByInterval[stepsByInterval$x == max(stepsByInterval$x),1]
```

```
## [1] 835
```


##Imputing missing values    

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

For filling in all of the missing values, I used the mean value for that 5-minute interval.


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
stepsByInterval <- aggregate(activityData$steps, by=list(interval=activityData$interval), FUN=mean,na.rm=TRUE)

newActivityData <- NULL
for(i in 1:nrow(activityData)) {
    row <- activityData[i,]
    if(is.na(row$steps)){
      row$steps <- stepsByInterval[stepsByInterval$interval == row$interval,2]
    }
    newActivityData <- rbind(newActivityData,row)
}

dim(newActivityData)
```

```
## [1] 17568     3
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsByDate <- aggregate(newActivityData$steps, by=list(date=newActivityData$date), FUN=sum, na.rm=TRUE)
hist(stepsByDate$x,
    breaks=10,
    col = "red",
    main = "Histogram of the total number of steps taken each day",
    xlab = "Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)

```r
mean(stepsByDate[["x"]])
```

```
## [1] 10766.19
```

```r
median(stepsByDate[["x"]])
```

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend indicating whether a given date is a weekday or weekend day.


```r
newActivityData$day <- ifelse(weekdays(as.Date(newActivityData$date)) %in% c("Saturday","Sunday"),"weekend", "weekday")
head(newActivityData)
```

```
##       steps       date interval     day
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
stepsByInterval <- aggregate(newActivityData[1], by=newActivityData[c(3,4)], FUN=mean, na.rm=TRUE)

library(ggplot2)
plot <- ggplot(data = stepsByInterval, aes(x=interval,y=steps))
plot + geom_line() + facet_wrap(~day,nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)
