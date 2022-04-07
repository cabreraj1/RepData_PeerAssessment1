Reproducible Research: Peer Assessment 1
================================================================  


Loading and preprocessing the data  
----------------------------------------------------------------

### 1. Load the data  

```{r echo=TRUE}
activity = read.csv("activity.csv")
```

What is mean total number of steps taken per day?
----------------------------------------------------------------

### 1. Calculate the total number of steps taken per day  

```{r echo=TRUE}
stepsPerDay <- aggregate(steps~date, data=activity, sum, na.rm=TRUE)
```

### 2. Make a histogram of the total number of steps taken each day

```{r echo=TRUE}
hist(stepsPerDay$steps, main="Total number of steps per day", xlab="Steps", col="blue", breaks=8)
```

### 3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
meanStepsPerDay <- mean(stepsPerDay$steps)
medianStepsPerDay <- median(stepsPerDay$steps)
```

```{r echo=TRUE}
meanStepsPerDay
medianStepsPerDay
```

What is the average daily activity pattern?
----------------------------------------------------------------

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r echo=TRUE}
stepsInterval <- aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
plot(stepsInterval$interval, stepsInterval$steps, type="l", xlab="5-min interval", ylab="Average steps", main="Average Daily Activity Pattern", col="blue")
```

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r echo=TRUE}
stepsInterval$interval[which.max(stepsInterval$steps)]
```

Imputing missing values
----------------------------------------------------------------------

### 1. Calculate and report the total number of missing values in the dataset.

```{r echo=TRUE}
nrow(activity[is.na(activity$steps),])
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r echo=TRUE}
activityWithoutNAs <- activity
activityWithoutNAs[is.na(activityWithoutNAs$steps), "steps"] <- 0
```

### 3. Make a histogram of the total number of steps taken each day.

```{r echo=TRUE}
stepsPerDayWithoutNAs <- aggregate(steps~date, data=activityWithoutNAs, sum, na.rm=TRUE)
hist(stepsPerDayWithoutNAs$steps, main="Total number of steps per day without NAs", xlab="Steps", col="blue", breaks=8)
```

### 4. Calculate and report the mean and median total number of steps taken per day. 

```{r}
meanStepsPerDayWithoutNAs <- mean(stepsPerDayWithoutNAs$steps)
medianStepsPerDayWithoutNAs <- median(stepsPerDayWithoutNAs$steps)
```

```{r echo=TRUE}
meanStepsPerDayWithoutNAs
medianStepsPerDayWithoutNAs
```

### 5. Do these values differ from the estimates from the first part of the assignment? 

Yes

### 6. What is the impact of imputing missing data on the estimates of the total daily number of steps?

Since the strategy was to change the NAs for 0 steps then the mean and median both decreased. 

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------------

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend”.

```{r echo=TRUE}
activityWithoutNAs$day <- as.POSIXlt(activityWithoutNAs$date)$wday
activityWithoutNAs$dayType <- as.factor(ifelse(activityWithoutNAs$day == 0 | activityWithoutNAs$day == 6, "weekend", "weekday"))
```

```{r echo=TRUE}
head(activityWithoutNAs)
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r echo=TRUE}
weekdaysData <- activityWithoutNAs[activityWithoutNAs$dayType == "weekday",]
weekendsData <- activityWithoutNAs[activityWithoutNAs$dayType == "weekend",]
stepsIntervalWeekdays <- aggregate(steps ~ interval, weekdaysData, mean)
stepsIntervalWeekends <- aggregate(steps ~ interval, weekendsData, mean)

par(mfrow = c(2, 1))

plot(stepsIntervalWeekdays, type = "l", col = "blue", main = "Weekdays")
plot(stepsIntervalWeekends, type = "l", col = "magenta", main = "Weekends")
```
