## Loading and preprocessing the data
# Downloading & extracting source data file
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile = "activity.zip")
unzip("activity.zip")

# Loading any required libraries
library(ggplot2)
library(knitr)
library(lattice)

# Reading in, exploring, and cleaning the data
data <- read.csv("activity.csv")
dim(data)
str(data)
summary(data)
head(data)

# remove all NA data
dataComplete <- na.omit(data)
summary(dataComplete)



## What is mean total number of steps taken per day?
# Calculate the total number of steps taken per day
dataSteps <- aggregate(steps ~ date, dataComplete, sum)

# Make a histogram of the total number of steps taken each day
hist(dataSteps$steps, col="blue", main="Histogram of total number of steps per day", xlab="Total number of steps in a day")

# Calculate and report the mean and median of the total number of steps taken per day
mean(dataSteps$steps)
median(dataSteps$steps)



## What is the average daily activity pattern?
# Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
dataIntervalSteps <- aggregate(steps ~ interval, dataComplete, mean)
str(dataIntervalSteps)
with(dataIntervalSteps, plot(interval, steps, type='l', col="brown", main="Average number of steps by 5-minute interval", xlab="5-minute Interval", ylab="Average number of steps"))

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
dataIntervalSteps[(which.max(dataIntervalSteps$steps)),]



## Imputing missing values
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
nrow(data[!complete.cases(data),])


# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# Perform the imputation by looping through the rows and replacing NA with the mean value
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
dataFilled  <- data
for (i in 1:nrow(dataFilled))
{
  if (is.na(dataFilled$steps[i]))
  {
    x <- dataIntervalSteps$steps[dataIntervalSteps$interval == dataFilled$interval[i]];
    dataFilled$steps[i] <- x;
  }
}
sum(is.na(dataFilled))
str(dataFilled$steps)

# Make a histogram of the total number of steps taken each day
dataImputedSteps <- aggregate(steps ~ date, dataFilled, sum)
hist(dataImputedSteps$steps, col="grey", main="Histogram of total number of steps per day (Imputed)", xlab="Total number of steps per day")

# Calculate and report the mean and median total number of steps taken per day.
mean(dataImputedSteps$steps)
median(dataImputedSteps$steps)

### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
### Answer: The mean remains unchanged, but there is a very slight difference with the median. total number of steps per day is increased.


## Are there differences in activity patterns between weekdays and weekends?
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
dataFilled$day <- "weekday"
dataFilled$day[weekdays(as.Date(dataFilled$date), abb=T) %in% c("Sat","Sun")] <- "weekend"
table(dataFilled$day)

# Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
dataFilledMean <- aggregate(steps ~ interval + day, data=dataFilled, FUN="mean")
ggplot(dataFilledMean, aes(x=interval, y=steps, color=day)) + geom_line() + labs(title="Average Daily Steps: Weekday vs Weekend", x="Interval", y="Number of Steps") + facet_wrap(~day, ncol=2, nrow=2)