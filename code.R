rm(list=ls())
## Loading and preprocessing the data

df.original <- read.csv(file="./activity.csv" )

df <- df.original
df$date <- as.POSIXct(as.character(df$date), format="%Y-%m-%d")


## What is mean total number of steps taken per day?
## 1) Make a histogram of the total number of steps taken each day

### grouping and computing the total steps taken per day!
library(dplyr)
df <- df[!is.na(df$steps),]#remove NA rows resulting in 8 lesser days (53<--61)
mn0 <- df %>% group_by(date) %>% summarize(num.steps.per.day=sum(steps))

hist(mn0$num.steps.per.day,main="Histogram of Total number of steps taken each day",xlab="Total number of steps take in a day")

## 2)Calculate and report the mean and median total number of steps taken per day

summary(mn0$num.steps.per.day)
m <- round(mean(mn0$num.steps.per.day, na.rm=TRUE))
n <- median(mn0$num.steps.per.day, na.rm=TRUE)
## What is the average daily activity pattern?

## 1) Make a time series plot (i.e. type = "l") of the 5-minute
## interval (x-axis) and the average number of steps taken, averaged
## across all days (y-axis)

### data adhesion mean mutate


### convert the interval to actual time


mn1 <- df %>% group_by(interval) %>% summarize(num.of.steps.per.day=mean(steps))

                                        # convert mn1 interval to DT
                                        # subtract!

mn1$interval <- as.character(mn1.interval)
mn1$interval <- str_pad(mn1$interval, width=4, side="left", pad="0")
mn1$interval <- strptime(mn1$interval,"%H%M")
mn1
mn1$actualInterval <- mn1$interval-mn1$interval[1]
mna$actualInterval <- as.numeric(mn1$actualInterval)

plot(mn1$actualInterval/60, mn1$num.of.steps.per.day,type="l",
     xlab="Time in minutes starting from 00:00",main="Time series plot",ylab="Avg. numver of steps taken (across all days for given time interval)")

### Which 5-minute interval, on average across all the days in the
### dataset, contains the maximum number of steps?

ind <- which.max(mn1$num.of.steps.per.day)
hrs <- as.numeric(mn1$actualInterval[ind])%/%3600
mins <- as.numeric(mn1$actualInterval[ind])%%3600/60
interval <- paste(hrs,":",mins)

## Imputing missing values
### Calculate and report the total number of missing values in the
### dataset
df <- df.original # reset the data
sum(is.na(df))
sum(is.na(df$steps))

### Devise a strategy for filling in all of the missing values in the
### dataset. The strategy does not need to be sophisticated. For
### example, you could use the mean/median for that day, or the mean
### for that 5-minute interval, etc.

#### We are expected to calculate the total number of steps per
#### day. The strategy that is chosen is the one where the NA values
#### are imputed with the mean of the interval (averaged across all
#### days)

#### Steps during the same time across days is expected to be similar
#### than steps during the entire day. For example: People sleep for 8
#### hrs in a day and across all the days it is expected that the
#### steps is same during sleep!

### 3. Create a new dataset that is equal to the original dataset but
### with the missing data filled in.

df <- df.original

mn2 <- df %>% group_by(interval) %>% mutate(mean.across.days=mean(steps,na.rm=TRUE))
## Are there differences in activity patterns between weekdays and
## weekends?

na.row <- which(is.na(mn2$steps),arr.ind=TRUE)
mn2$steps.imputed <- mn2$steps
mn2$steps.imputed[na.row] <- mn2$mean.across.days[na.row]

### 4. Histogram
library(dplyr)
mn3 <- mn2 %>% group_by(date) %>% summarize(num.steps.per.day=sum(steps.imputed))
hist(mn3$num.steps.per.day,main="Histogram of Total number of steps taken each day",xlab="Total number of steps take in a day")

### 4. mena and median
summary(mn3$num.steps.per.day)
mean(mn3$num.steps.per.day)
median(mn3$num.steps.per.day)

summary(mn0$num.steps.per.day)

### Do thes values differ from the estimates from the first part of
### the assignment? As seen above not much

### What is the impact of imputing missing data on the estimates fo
### the total daily number of steps? As seen above, not much in the
### mean and median. A bit of difference in the 1st and 3rd quartile.
### It doesn't differ much because only 13% of the data is missing,
### and the approximation didn't add 

## Are there differences in activity patterns between weekdays adn
## weekends?

### 1. Create a new factor variable in the dataset with two levels --
### "weekday" and "weekend" indicating whether a given date is a
### weekday or weekend day.

df <- mn2
df$daynum <- as.POSIXlt(as.character(df$date))$wday 
df$daytype <- df$daynum
df$daytype[df$daynum %in% c(0,6)] <- "wkend"
df$daytype[!df$daynum %in% c(0,6)] <- "wkday"
df[sample(1:nrow(df),size=100),]
df$daytype <- as.factor(df$daytype)

### 2.
library(lattice)
mn <- df %>% group_by(interval,daytype) %>%
    summarize(num.of.steps.per.interval=mean(
                  steps.imputed))
xyplot(num.of.steps.per.interval~interval|daytype,data=mn,type="l", layout=c(1,2))
