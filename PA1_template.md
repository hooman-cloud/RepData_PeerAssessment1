---
title: "PA1_template"
output: 
  html_document:
    self_contained: FALSE
    keep_md: TRUE
---

# Load the data


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.0.4
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
library(chron)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.4
```


```r
rawData <- readr::read_csv(unzip("activity.zip", "activity.csv"))
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
# head(rawData)
# names(rawData)
# summary(rawData)
# str(rawData)
# glimpse(rawData)
```
# Remove the NA rows


```r
data <- rawData[!is.na(rawData$steps),]
```

# Calculate the total number of steps taken per day


```r
stepsPerDay <- data %>% group_by(date) %>% summarise(sum(steps, na.rm = TRUE))
stepsPerDay <- data.frame(stepsPerDay)
names(stepsPerDay) <- c('date', 'totalSteps')
```

# Make a histogram of the total number of steps taken each day


```r
hist(stepsPerDay$totalSteps, breaks = 5, main="Histogram of total number of steps taken each day", xlab="Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

# Calculate and report the mean and median of the total number of steps taken per day


```r
mean(stepsPerDay$totalSteps)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$totalSteps)
```

```
## [1] 10765
```

# Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsPerInterval1 <- data %>% group_by(interval) %>% summarise(mean(steps, na.rm = TRUE))
stepsPerInterval2 <- aggregate(data$steps ~ data$interval, FUN = mean)
names(stepsPerInterval2) <- c("interval", "averageSteps")
with(stepsPerInterval2, plot(interval, averageSteps, type = 'l',
                              main="Average number of steps",                                    xlab="5-minute interval", ylab="Average number of steps taken"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsPerInterval2[which.max(stepsPerInterval2$averageSteps), "interval"]
```

```
## [1] 835
```

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 


```r
sum(is.na(rawData))
```

```
## [1] 2304
```

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in.



```r
missingValuesIndices <- as.data.frame(which(is.na(rawData), arr.ind = TRUE))
rawDataImputed <- rawData
for (i in missingValuesIndices[,1]) {
  
  tempInterval <- rawData[i, 3]
  tempIntervalMean <- stepsPerInterval2[which(stepsPerInterval2[,1] %in% tempInterval), 2]
  rawDataImputed[i,1] <- tempIntervalMean
  
}
```

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsPerDayImputed <- aggregate(rawDataImputed$steps,list(rawDataImputed$date), sum)
names(stepsPerDayImputed) <- c("date", "totalSteps")
hist(stepsPerDayImputed$totalSteps, breaks = 5,main="Histogram of total number of steps take each day", 
     xlab="Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
mean(stepsPerDayImputed$totalSteps)
```

```
## [1] 10766.19
```

```r
median(stepsPerDayImputed$totalSteps)
```

```
## [1] 10766.19
```

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
rawDataImputed[,4] <- factor(is.weekend(rawDataImputed$date), labels = c("weekday", "weekend"))
```

# Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
stepsPerIntervalWeekday <- aggregate(rawDataImputed$steps,list(rawDataImputed$interval, 
                                                               rawDataImputed$...4), mean)

names(stepsPerIntervalWeekday) <- c("interval", "weekend", "averageSteps")

plot <- ggplot(stepsPerIntervalWeekday, aes(interval, averageSteps)) +
  geom_line(stat = "identity", aes(colour = weekend)) +
  facet_grid(weekend ~ ., scales="fixed", space="fixed") 

print(plot)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

