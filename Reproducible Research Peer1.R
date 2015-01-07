# preparing the data for processing
# upzip file and read the data file named : "activity.csv"
## resend for repdata-010 (Jan 2015)

unzip("repdata_data_activity.zip")
origData = read.csv("activity.csv")
# removal of the NAs in the original data.
RefineData = na.omit(origData)

# Step 1: What is mean total number of steps taken per day?
# Find the mean and median of the total number of steps taken per day.
# Step 1.1: using ddply to count the steps according to each date.
library(plyr)
totalSteps.Day = ddply(RefineData, .(date), summarise, steps=sum(steps))
# Step 1.2: Make a histogram of the total number of steps taken each day.
hist(totalSteps.Day$steps, breaks = 30, main="Total No. of steps taken per day", 
     xlab="Total No. of steps", ylab = "No. of Days", col="grey")
# Step 1.3: calculate the mean and median of the totalSteps.Day
mean(totalSteps.Day$steps)
median(totalSteps.Day$steps)

# Step 2.1: Make a time series plot (i.e. type = "l") of the 5-minute interval 
#(x-axis) and the average number of steps taken, 
#averaged across all days (y-axis)
# Step 2.1.1 : using ddply to allocate the intervals to the ave steps taken
avePerInt = ddply(RefineData , .(interval), summarise, steps=mean(steps))
# Step 2.1.2 : using ggplot to plot out the graph
library(ggplot2)
ggplot(data=avePerInt, aes(x=interval, y=steps)) +
    geom_line(color = "red", size = 1.0) +
    labs(title = "Time Series Plot of the 5-minute Intervals",
             x = "5-minute interval",
             y = "average number of steps taken")
# Step 2.2 : Which 5-minute interval, on average across all the days in the 
# dataset, contains the maximum number of steps?
# Step 2.2.1 : using the which.max subset to find the value
avePerInt[which.max(avePerInt$steps),]

# Step 3.1: Calculate and report the total number of missing values in the dataset
#(i.e. the total number of rows with NAs)
# Step 3.1.1 : using the Sum function on the original Data.
sum(is.na(origData))
# Step 3.2: Devise a strategy for filling in all of the missing values in the 
# dataset. The strategy does not need to be sophisticated. For example, you 
# could use the mean/median for that day, or the mean for that 5-minute interval
# Step 3.2.1 : filling the missing data with mean values of that 5min interval
# and reate a new dataset that is equal to the original dataset but 
# with the missing data filled in.
NuData = origData
    for (i in 1:nrow(NuData)) {
            if (is.na(NuData$steps[i])) {
            NuData$steps[i] <- avePerInt$steps[which(NuData$interval[i] 
            == avePerInt$interval)]
        }
    }
# Step 3.3: Make a histogram of the total number of steps taken each day and 
# Calculate and report the mean and median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the 
#assignment? What is the impact of imputing missing data on the estimates of the
# total daily number of steps?
# Step 3.3.1: Using ddply to calculate the total number of steps / Day
NtotalSteps.Day = ddply(NuData, .(date), summarise, steps=sum(steps))
hist(NtotalSteps.Day$steps, breaks = 30, main="Total No. of steps taken per day"
     , xlab="Total No. of steps", ylab = "No. of Days", col="green")
# Step 3.3.2: Mean and median of the total number of steps taken per day.
mean(NtotalSteps.Day$steps)
median(NtotalSteps.Day$steps)
# Step 3.3.3: Comparing data from first part to second part.
mean(NtotalSteps.Day$steps) - mean(totalSteps.Day$steps)
median(NtotalSteps.Day$steps) - median(totalSteps.Day$steps)
# the mean between both are the same. However the second median has 1.188679
# more than the first.

# Step 4.1: Create a new factor variable in the dataset with two levels 
# – “weekday” and “weekend” indicating whether a given date is a weekday or 
# weekend day.
# Step 4.1.1: Using Sys.setlocale to set the days of the week from monday to 
# sunday, and create a weekday col for the NuData.
Sys.setlocale("LC_TIME")
NuData$weekdays <- weekdays(as.Date(NuData$date))
# Step 4.1.2: change the monday to sunday into weekdays / weekends using ifelse
NuData$weekdays <- ifelse(NuData$weekdays %in% c("Saturday", "Sunday"),
                    "weekend", "weekday")
# Step 4.2: Make a panel plot containing a time series plot (i.e. type = "l") of
# the 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis).
# Step 4.2.1: Using ddply we sort out the data for plotting.
aveWeekDE <- ddply(NuData, .(interval, weekdays), summarise, steps=mean(steps))
# Step 4.2.2: Making the panel plot
library(lattice)
xyplot(steps ~ interval | weekdays, data = aveWeekDE, 
       layout = c(1, 2), type="l",
       xlab = "Interval", ylab = "Number of steps")