---
title: "activity_data_Souvik"
author: "ss"
date: "November 27, 2017"
output: html_document
---
##What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day

```r
full_data<-read.csv("steps_data/activity.csv")
full_data$date<-as.Date(levels(full_data$date)[full_data$date])
total_steps_a_day<-aggregate(steps~date,full_data,sum,na.rm=T)
head(total_steps_a_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
tail(total_steps_a_day)
```

```
##          date steps
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

```r
#Histogram of total steps taken per day
hist(total_steps_a_day$steps,xlab = "Total steps taken each day",main="Histogram of total steps taken each day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
# Mean and median of total steps taken per day
mean(total_steps_a_day$steps)
```

```
## [1] 10766.19
```

```r
median(total_steps_a_day$steps)
```

```
## [1] 10765
```
##What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
mean_data<-aggregate(steps~interval,full_data,mean)
plot(mean_data$interval,mean_data$steps,type = "l",xlab = "Interval",ylab = "Mean number of steps",main = "Mean number of steps per day by interval")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
#The interval with maximum steps
mean_data[which.max(mean_data$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```
##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# Total number of rows with NA's
nrow(full_data[complete.cases(full_data),])
```

```
## [1] 15264
```
We can replace the missing values with the average value of the steps per 5-minute intervals.




```r
index<-which(is.na(full_data$steps))
mean_steps<-with(full_data,tapply(steps,interval,mean,na.rm=T))
m<-mean(mean_steps,na.rm = T)
for(i in 1:length(index)){
    full_data[index[i],1]<-m
}
# Checking if there are any NA's
sum(any(is.na(full_data)))
```

```
## [1] 0
```


```r
total_steps_a_day<-aggregate(steps~date,full_data,sum,na.rm=T)
#Histogram of total steps taken per day
hist(total_steps_a_day$steps,xlab = "Total steps taken each day",main="Histogram of total steps taken each day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
# Mean and median of total steps taken per day
mean(total_steps_a_day$steps)
```

```
## [1] 10766.19
```

```r
median(total_steps_a_day$steps)
```

```
## [1] 10766.19
```
##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
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
full_data<- mutate(full_data, day = ifelse(weekdays(full_data$date) == "Saturday" | weekdays(full_data$date) == "Sunday", "weekend", "weekday"))
full_data$day<-as.factor(full_data$day)
head(full_data)
```

```
##     steps       date interval     day
## 1 37.3826 2012-10-01        0 weekday
## 2 37.3826 2012-10-01        5 weekday
## 3 37.3826 2012-10-01       10 weekday
## 4 37.3826 2012-10-01       15 weekday
## 5 37.3826 2012-10-01       20 weekday
## 6 37.3826 2012-10-01       25 weekday
```

```r
data_weekend<-subset(full_data,full_data$day=="weekend")
data_weekday<-subset(full_data,full_data$day=="weekday")
mean_steps_weekend<-tapply(data_weekend$steps,data_weekend$interval,mean)
mean_steps_weekday<-tapply(data_weekday$steps,data_weekday$interval,mean)
weekend<-unique(data_weekend$interval)
weekday<-unique(data_weekday$interval)
final_weekend<-data.frame(cbind(mean_steps_weekend,weekend))
final_weekday<-data.frame(cbind(mean_steps_weekday,weekday))
par(mfrow=c(2,1),mar=c(4,4,2,1))
plot(final_weekend$weekend,final_weekend$mean_steps_weekend,type = "l",xlab = "Intervals",ylab = "Average Steps",main = "Weekend")
plot(final_weekday$weekday,final_weekday$mean_steps_weekday,type = "l",xlab = "Intervals",ylab = "Average Steps",main = "Weekday")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)


