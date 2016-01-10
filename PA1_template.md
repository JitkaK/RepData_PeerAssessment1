---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
"Reproducible Research: Peer Assessment 1"
=============================================

## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

Firstly a vector of total number of steps taken each day is created and then the histogram:

```r
tot_steps <- sapply(split(data$steps,data$date),sum,na.rm=T)
hist(tot_steps,main="Histogram of the total number of steps taken each day",xlab="Number of steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

After that mean and median total steps taken each day is computed:

```r
mean(tot_steps)
```

```
## [1] 9354.23
```

```r
median(tot_steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

In this section we create a time series plot of average number of steps taken every day through each 5-minute intervals. Firstly, we have to count the average using `sapply` function on the list of intervals:

```r
Int <- split(data$steps,data$interval) #list of intervals
mean_day_intervals <- sapply(Int,mean,na.rm=T)
```

And then plot the time series

```r
plot(mean_day_intervals,type="l",main="Time series of average number of steps",xlab="5-minute intervals",ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

We find the maximal value of average number of steps taken every day with the variable `max`:

```r
max <- which(mean_day_intervals==max(mean_day_intervals))
```


To find which interval is the maximal one, we call the variable max as numeric:

```r
as.numeric(max)
```

```
## [1] 104
```

We could show the exact time when the interval begins:

```r
time <- as.numeric(names(max))
paste(time%/%100,time%%100,sep=":")
```

```
## [1] "8:35"
```


## Imputing missing values

Finding how many of NA values are in dataset:

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

We will use the median of 5-minute intervals for fill in NA values. Firstly, variable `median_day_interval` is made:

```r
median_day_intervals <- sapply(Int,median,na.rm=T)
```

Then we create the second dataset

```r
data2 <- data
```

And the new vector without NA values is bulit in a for-loop 

```r
all_time <- as.numeric(names(Int)) 
Y<-matrix(0,length(all_time),length(levels(data$date)))
for (i in 1:length(all_time)){
    x<-data$steps[data$interval==all_time[i]]
    x[is.na(x)]<-median_day_intervals[i]
    Y[i,]<-x
}
y <- c(Y)
```

This vector replace the first columnt of the new dataset

```r
data2$steps <- y
```

Now we compare the new dataset using histogram

```r
tot_steps2 <- sapply(split(data2$steps,data2$date),sum,na.rm=T)
hist(tot_steps2,main="Histogram of the total number of steps taken each day",sub="after replacing NA values",xlab="Number of steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)

and mean and median total steps taken each day:

```r
mean(tot_steps2)
```

```
## [1] 9503.869
```

```r
median(tot_steps2)
```

```
## [1] 10395
```

Are they the same?

```r
mean(tot_steps2)==mean(tot_steps)
```

```
## [1] FALSE
```

```r
median(tot_steps2)==median(tot_steps)
```

```
## [1] TRUE
```

```r
hist(tot_steps,col=rgb(0,0,1,1/4))        # this histogram is light blue             
hist(tot_steps2, col=rgb(1,0,0,1/4), add=T) # this histogram is pink
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)

```r
# combination creates purple color
```

Replacing of NA values with median changes the mean but histogram and median of steps remain the same.

## Are there differences in activity patterns between weekdays and weekends?

At first we have to change the dates into Date class:

```r
date <- lapply(data2$date,as.Date,"%Y-%m-%d")
```

Then we have to detect the number of the day (1 for "Monday", 2 for "Tuesday",...)

```r
num_d <- sapply(date,format,"%w")
#I didn't use the function 'weekdays()' because it returned the name of a day in my language and I wasn't sure if it would work somewhere else.
```

Finally we have to detect which day is a weekday and which is a weekend:

```r
day <- rep("weekday",dim(data2)[1])
day[num_d==0] <- "weekend"
day[num_d==6] <- "weekend"
data2$day <- day
```

Average number of steps in every interval for weekdays and weekends:

```r
Week <- split(data2[,c(1,3)],data2$day)
Wday <- Week$weekday
Wend <- Week$weekend
Int_d <- split(Wday$steps,Wday$interval)
Int_e <- split(Wend$steps,Wend$interval)
mean_day <-sapply(Int_d,mean,na.rm=T)
mean_end <-sapply(Int_e,mean,na.rm=T)
```

Combined plot of a time series:

```r
m<-rbind(c(1),c(2))
layout(m)
par(mar = c(0, 2.5, 3, 1))
plot(mean_day,type="l",main="Weekdays",xaxt="n")
par(mar = c(3, 2.5, 3, 1))
plot(mean_end,type="l",main="Weekends")
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png)

