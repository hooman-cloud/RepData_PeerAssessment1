# Load the data
# 
library(dplyr)
library(chron)
library(ggplot2)

rawData <- readr::read_csv(unzip("activity.zip", "activity.csv"))
head(rawData)
names(rawData)
summary(rawData)
str(rawData)
glimpse(rawData)
# Remove the NA rows
data <- rawData[!is.na(rawData$steps),]

# Calculate the total number of steps taken per day

stepsPerDay <- data %>% group_by(date) %>% summarise(sum(steps, na.rm = TRUE))
stepsPerDay <- data.frame(stepsPerDay)
names(stepsPerDay) <- c('date', 'totalSteps')

# If you do not understand the difference between a histogram and a barplot, 
# research the difference between them. Make a histogram of the total number of 
# steps taken each day

hist(stepsPerDay$totalSteps, breaks = 5)

# Calculate and report the mean and median of the total number of steps taken 
# per day

mean(stepsPerDay$totalSteps)
median(stepsPerDay$totalSteps)

# 
# Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the
# 5-minute interval (x-axis) and the average number of steps taken, averaged
# across all days (y-axis)
# 

stepsPerInterval1 <- data %>% group_by(interval) %>% summarise(mean(steps, na.rm = TRUE))
stepsPerInterval2 <- aggregate(data$steps ~ data$interval, FUN = mean)
names(stepsPerInterval2) <- c("interval", "averageSteps")
with(stepsPerInterval2, plot(interval, averageSteps, type = 'l'))

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

stepsPerInterval2[which.max(stepsPerInterval2$averageSteps), "interval"]

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with 

sum(is.na(rawData))

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example,
# you could use the mean/median for that day, or the mean for that 5-minute 
# interval, etc.
# Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.

# 

missingValuesIndices <- as.data.frame(which(is.na(rawData), arr.ind = TRUE))
rawDataImputed <- rawData
for (i in missingValuesIndices[,1]) {
  
  tempInterval <- rawData[i, 3]
  tempIntervalMean <- stepsPerInterval2[which(stepsPerInterval2[,1] %in% tempInterval), 2]
  rawDataImputed[i,1] <- tempIntervalMean
  
}

# Make a histogram of the total number of steps taken each day and Calculate and 
# report the mean and median total number of steps taken per day. Do these values 
# from the estimates from the first part of the assignment? What is the impact of
# imputing missing data on the estimates of the total daily number of steps?

stepsPerDayImputed <- aggregate(rawDataImputed$steps,list(rawDataImputed$date), sum)
names(stepsPerDayImputed) <- c("date", "totalSteps")
hist(stepsPerDayImputed$totalSteps, breaks = 5)
mean(stepsPerDayImputed$totalSteps)
median(stepsPerDayImputed$totalSteps)

# Create a new factor variable in the dataset with two levels – “weekday” and 
# “weekend” indicating whether a given date is a weekday or weekend day.
# 
#rawDataImputed <- as.data.frame(rawDataImputed)
rawDataImputed[,4] <- factor(is.weekend(rawDataImputed$date), labels = c("weekday", "weekend"))

# Make a panel plot containing a time series plot 
# (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval
# (x-axis) and the average number of steps taken, averaged across all weekday 
# days or weekend days (y-axis). See the README file in the GitHub repository 
# to see an example of what this plot should look like using simulated data.
# 
# stepsPerIntervalImputedWeekend <- aggregate(rawDataImputed$steps[rawDataImputed$...4 == "weekend"] ~ rawDataImputed$interval[rawDataImputed$...4 == "weekend"], FUN = mean)
# names(stepsPerIntervalImputedWeekend) <- c("interval", "averageSteps")
# 
# stepsPerIntervalImputedWeekday <- aggregate(rawDataImputed$steps[rawDataImputed$...4 == "weekend"] ~ rawDataImputed$interval[rawDataImputed$...4 == "weekend"], FUN = mean)
# names(stepsPerIntervalImputedWeekday) <- c("interval", "averageSteps")

stepsPerIntervalWeekday <- aggregate(rawDataImputed$steps,list(rawDataImputed$interval, 
                                                               rawDataImputed$...4), mean)

names(stepsPerIntervalWeekday) <- c("interval", "weekend", "averageSteps")

plot <- ggplot(stepsPerIntervalWeekday, aes(interval, averageSteps)) +
  geom_line(stat = "identity", aes(colour = weekend)) +
  facet_grid(weekend ~ ., scales="fixed", space="fixed") 

print(plot)