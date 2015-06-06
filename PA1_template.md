# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
act<-read.csv(unz("activity.zip","activity.csv"), header=TRUE, sep=",")
act$date<-as.Date(as.character(act$date),"%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
library(dplyr,warn.conflicts = FALSE)
by_date<-group_by(act,date)
dailysteps<-summarise(by_date,tsteps=sum(steps))
mean(dailysteps$tsteps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(dailysteps$tsteps,na.rm=TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
by_interval<-group_by(act,interval)
dayact<-summarise(by_interval, intsteps=mean(steps,na.rm=TRUE))
plot(dayact,type="l",ylab="Average steps",main="Daily activity")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
dayact[which.max(dayact$intsteps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval intsteps
## 1      835 206.1698
```
## Imputing missing values

```r
sum(is.na(act$steps))
```

```
## [1] 2304
```

```r
## mean for the 5 minute interval is used to fill NAs
actn<-act
actn$steps[is.na(act$steps)]<-dayact$intsteps[match(act$interval[is.na(act$steps)], dayact$interval)]
by_daten<-group_by(actn,date)
dailystepsn<-summarise(by_daten,tsteps=sum(steps))
mean(dailystepsn$tsteps)
```

```
## [1] 10766.19
```

```r
median(dailystepsn$tsteps)
```

```
## [1] 10766.19
```
### Mean has remained same, while Median has become equal to mean due to imputing missing values

## Are there differences in activity patterns between weekdays and weekends?

```r
library(ggplot2)
actn$daytype<-ifelse(weekdays(actn$date,abbreviate=TRUE) %in% c("Sat","Sun"), "weekend","weekday")
actn$daytype<-as.factor(actn$daytype)
by_intervaln<-group_by(actn,interval,daytype)
dayactn<-summarise(by_intervaln, intsteps=mean(steps,na.rm=TRUE))
ggplot(dayactn,aes(interval,intsteps,group=daytype))+geom_line()+facet_wrap(~daytype,ncol=1)+labs(y="Number of steps",title="Daily activity ")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
