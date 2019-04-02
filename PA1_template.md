---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    keep_md: yes
keep_md: true
---



## Loading and preprocessing the data


```r
repdata <- read.csv("./activity.csv")
repdata_1 <- na.omit (repdata)
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
library(ggplot2)
```

## Total number of steps per day

```r
result_total <- aggregate(repdata_1$steps~repdata_1$date,FUN = sum)
colnames(result_total) <- c("date","steps")
```
Histogram of daily steps number

```r
hist(result_total$steps,col = "red", main = "Daily steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Mean and median number of steps per day


```r
result_mean <-mean(result_total$steps)
result_median <-median(result_total$steps)
print(result_mean)
```

```
## [1] 10766.19
```

```r
print(result_median)
```

```
## [1] 10765
```

## Time series plots and the Max of step and relative intervals

```r
result_avg <- aggregate(repdata_1$steps~repdata_1$interval,FUN = mean)
colnames(result_avg) <-c("interval","steps")
step_max <-max(result_avg$steps)
interval_max <- result_avg$interval[result_avg$steps==step_max]
with(result_avg,plot(interval,steps,type="l",main="Average steps of every 5minutes in a day"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
print(interval_max)
```

```
## [1] 835
```

```r
print(step_max)
```

```
## [1] 206.1698
```

## Input missing data and Filling in NAs with average steps of 5 minutes interval

```r
nrow(repdata[is.na(repdata$steps),])
```

```
## [1] 2304
```
Filling in NAs with average steps of 5-minutes interval

```r
repdata_na<-repdata[is.na(repdata$steps),]
repdata_mg <-merge(repdata_na,result_avg,by = "interval")
repdata_fil<-repdata_mg[,c(4,3,1)]
colnames(repdata_fil)<-c("steps","date","interval")
repdata_new<-rbind(repdata_1,repdata_fil)
```
## Calculate the mean of new dataset and Plot histogram.

```r
result_new <- aggregate(repdata_new$steps~repdata_new$date,FUN = sum)
colnames(result_new)<-c("date","steps")
mean(result_total$steps)
```

```
## [1] 10766.19
```

```r
mean(result_new$steps)
```

```
## [1] 10766.19
```

```r
hist(result_new$steps,col = "red",main = "Daily steps per day adjusted")
hist(result_total$steps,col = "blue",add = T)
legend("topright",c("Adjusted Data","NA excluded data"),fill=c("red","blue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
New mean and median

```r
Newmean<-mean(result_new$steps)
Newmedian<-median(result_new$steps)
Newmean
```

```
## [1] 10766.19
```

```r
Newmedian
```

```
## [1] 10766.19
```
## Differences between weekdays and weekends

```r
library(lattice)
library(plyr)
```

```
## -------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.5.2
```

```r
repdata_new$day<-weekdays(as.Date(repdata_new$date))
repdata_new$wkd<-ifelse(repdata_new$day %in% c("星期六", "星期日"), "Weekend", "Weekday")
repdata_newAVG<-ddply(repdata_new,.(interval,wkd),summarize,Avg=mean(steps))
```
Plot the line figure using average number versus interval.


```r
xyplot(Avg~interval|wkd,data=repdata_newAVG,type="l",layout=c(1,2),main="Average Steps per Interval",ylab="Average Number of Steps",xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

