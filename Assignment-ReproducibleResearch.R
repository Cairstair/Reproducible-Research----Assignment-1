## Assignment: Course Project 1 [Reproducible Research]

# Collection of Data | Fitbit, Nike, Fuelband, or Jawbone Up.

# Required Packages Install

install.packages("knitr")

install.packages("dplyr")

install.packages("lubridate")

install.packages("ggplot2")

# Required Packages Load

library(dplyr)

library(lubridate)

library(ggplot2)

library("knitr")

opts_chunk$set(echo = TRUE)

# Reading Data Obtained From: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

data <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character", "integer"))

# Cleaning Data

data$date <- ymd(data$date)

# Clean Data Check

str(data)

head(data)

# Data is now clean and read to begin to the questions that have been put forward in this assignment. 

## Question 1: What is mean total number of steps taken per day?

# 1. Calculate the total number of steps taken per day 

# 2. If you do not understand the difference between a histogram and a barplot, research the difference 

#    between them. Make a histogram of the total number of steps taken each day 

#    [A start could be: http://stattrek.com/statistics/charts/histogram.aspx?Tutorial=AP]

# 3. Calculate and report the mean and median of the total number of steps taken per day.

# In order to do the above questions first lets calculate steps per day by using dplyr (should have

# have installed above) then group the output by date.

steps <- data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print

#Ouput: should be (Source: local data frame [53 x 2]) if other result double check csv.

#Then use ggplot to make the histogram (again should have been installed above)

ggplot (steps, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Histogram of Steps/Day", x = "Steps/Day", y = "Frequency")

#Now we need to take the mean & median of Steps/Day. 

mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)

#Output: should be (Mean: 10766.19 & Median: 10765) if alternative ensure you have gotten

#the correct csv.

## Question 2: What is the average daily activity pattern? 

# 1. What a time series plot (type = "l") of the 5-minute interval (x-axis) and the average number of steps

#    taken, averaged across all days (y-axis)?

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of

#    steps?

# In order to answer the above question we will calculate the average number of steps taken in each

# 5-minute interval/day using [dplyr] and group with [interval]. Graph with [ggplots]. Installed above.

interval <- data %>% 
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))

ggplot(interval, aes(x = interval, y = steps)) +
  geom_line(color = "Blue")

# Use [which.max] to establish the maximum steps (on average) across all of these days.

interval[which.max(interval$steps),]

# The interval should be 835 and steps should be 206.1698. If different ensure proper csv has been used.

## Question 3: Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 

#    NAs).

# 2. Devise a strategy for filling in all of the missing values in the dataset. 

#    The strategy does not need to be sophisticated. For example, you could use the mean/median for that 

#    day, or the mean for that 5-minute interval, etc.

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

# 4. Make a histogram of the total number of steps taken each day and calculate and report the mean 

#    and median total number of steps taken per day. Do these values differ from the estimates from the 

#    first part of the assignment? What is the impact of imputing missing data on the estimates of the 

#    total daily number of steps?

# First lets summarize all the missing values: 

sum(is.na(data$steps))

# This should equal 2304 if it doesn't ensure the proper csv is being used.

# Now we can work on we can work on resolving the NA values & create a new data set.

data_full <- data
nas <- is.na(data_full$steps)
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm = TRUE, simplify = TRUE)
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]

# That should have removed all NA values, but lets make sure. 

sum(is.na(data_full$steps))

# If above finished correctly then you should recieve a number in return of zero, if so, then we can

# make a summary.

steps_full <- data_full %>%
  filter(!is.na(steps))%>%
  group_by(date)%>%
  summarize(steps = sum(steps))%>%
  print

#(Source: local data frame [62 x 2]) 

# Then use ggplot (which was installed above) to create the histogram needed

ggplot(steps_full, aes(x = steps)) +
  geom_histogram(fill = "Blue", binwidth = 1000) +
  labs(title = "Histogram of Steps/Day (Including Missing Values)", x = "Steps/Day", y = "Frequency")

# Now we can calculate the mean/median steps with the filled in values

mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)

# mean_steps_full = 10766.19 & median_steps_full = 10766.19 [Yes they equal the same value, they are

# supposed to.]

## Question 4: Are there differences in activity patterns between weekdays & weekends?

# 1. Create a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating 

#    whether a given date is a weekday or weekend day.

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 

#    and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

# First we need to create a new column for weektype. This is done through using [dplyr] and [mutate].

data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "Saturday" | weekdays(data_full$date) == "Sunday", "weekend", "weekday"))
data_full$weektype <- as.factor(data_full$weektype)
head(data_full)

# Now we can calculate the average steps in the 5-minute interval. Lets use ggplot to compare the average

# steps for weeday and weekend. 

interval_full <- data_full %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))
s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)

# These show that there is more activity throughout the Weekends. It can be presumed that this could be 

# that individuals have more free time during the weekend. However, without further data that can not be

# proven.