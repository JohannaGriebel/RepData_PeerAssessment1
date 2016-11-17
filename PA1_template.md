# Reproducible Research: Peer Assessment 1
first loading of neccesary packages are required


```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(ggplot2)
library(plyr)
```

```
## 
## Attaching package: 'plyr'
```

```
## The following object is masked from 'package:lubridate':
## 
##     here
```
## Loading and preprocessing the data

loading the data into the R envrionment
'''{r}
activity<-read.csv("activity.csv")
'''
transform the dates to class date

'''{r}
activity$date<-ymd(activity$date)
'''

## What is mean total number of steps taken per day?

missing values are ignored for this part of the analysis

calculation of the total number of steps taken each day
'''{r}
stepsperday<-aggregate(steps~date, activity ,sum)
'''
histogram of the total number of steps taken per day
'''{r}
ggplot(df, aes(steps))+geom_histogram(bins=30)
'''
mean and median of total numbers of steps taken per day

'''{r}
meansteps<-mean(stepsperday$steps, na.rm =TRUE)
mediansteps<-median(stepsperday$steps, na.rm=TRUE)
data.frame(mean=meansteps, median=mediansteps)
'''

## What is the average daily activity pattern?
calculation of the total number of steps taken each interval
'''{r}
interval<-aggregate(steps~interval,activity, mean, na.rm=TRUE)
'''
time series plot of the interval and the average steps taken
'''{r}
ggplot(interval, aes(x=interval, y=steps))+geom_line()
'''
5-minute interval with maximum average number of steps
'''{r}
sapply(interval, max)
'''

## Imputing missing values
number of missing values in the data set (total number of rows with NA)
'''{r}
NNAs<-count(complete.cases(activity))
NNAs[1,2] 
'''
function to replace NA with the mean
'''{r}
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
'''
replace the NAs with the mean of steps of the corresponding 5-minute interval
'''{r}
activityNA <- ddply(activity, ~ interval, transform, steps=impute.mean(steps))
'''
combining the datasets of data with NA and without NA
'''{r}
stepsperday$group <- "with NA"
stepsperdayNA$group <- "without NA"
dfboth <- rbind(stepsperday,stepsperdayNA)
'''
histogram for the data with and without NA
'''{r}
ggplot(dfboth, aes(x=steps)) +
        geom_histogram(bins=30) +
        facet_grid(.~group)
'''
comparison of median and mean of the data with NAs and without NAs
'''{r}
meanstepsNA<-mean(stepsperdayNA$steps, na.rm =TRUE)
medianstepsNA<-median(stepsperdayNA$steps, na.rm=TRUE)
tabNA<-data.frame(mean=c(meansteps,meanstepsNA), median=c(mediansteps, medianstepsNA), row.names=c("with NA", "whithout NA"))
tabNA
'''
* imputing of the missing values with the mean of steps did not have an effect on the mean but had a small effect on the median
* the main impact lays more in higher counts on the value that represents the mean in the data

## Are there differences in activity patterns between weekdays and weekends?

translation of the dates to weekday
'''{r}
activityNA$weekdays<-weekdays(activityNA$date)
'''
create a new variable weekend
'''{r}
activityNA$weekend<-1
'''
fill in the new variable with weekday and weekend corresponding to the day of the week
(not the german words for the weekdays)
'''{r}
for (i in 1:17568) {
        if (activityNA[i,4]=="Montag" | 
            activityNA[i,4]=="Dienstag" | 
            activityNA[i,4]=="Mittwoch" | 
            activityNA[i,4]=="Donnerstag" |
            activityNA[i,4]=="Freitag") 
            {activityNA[i,5]<-c("weekday")}
        else {activityNA[i,5]<-c("weekend")}
        }
'''

caculate the average number of steps for weekdays and weekends for each 5-minute interval
'''{r}
activityNA$weekend<-as.factor(activityNA$weekend)
df<-aggregate(steps~weekend+interval, activityNA, mean)
'''
time series plots of the 5-minute interval and the average number of steps taken across weekdays and weekends
'''{r}
ggplot(df, aes(x=interval, y=steps))+geom_line()+facet_grid(weekend~.)
'''

