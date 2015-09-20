---
title: "Reproducible Research Report"
author: "Rustam Mansyrov"
date: "Sunday, September 20, 2015"
output: html_document
---

# Introduction

To begin with, the project is elaborated on data describing personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. The data consists of 5 minute intervals during the day, steps taken in each interval and date which interval belongs to. The data was collected by anonimous and the period of collecting it is two months, namely, October and November, 2012.

As requested, there are very interesting questions posed to be answered in this report.

* What is mean total number of steps taken per day?
* What is the average daily activity pattern?
* Imputing missing values, and re-answering the first question.
* Are there differences in activity patterns between weekdays and weekends?

The analysis is made using R-statistical software and the respective codes are provided below so that the whole analysis is entirely reproducible.

# Analysis

Before starting working on the data, the necessary preprocessing along with R codes are summarized below. 

```{r, echo = TRUE, results = 'hide', warning = FALSE, message = FALSE}
library(lubridate)
library(dplyr)
library(ggplot2)
dir.create("project_rr")
setwd("./project_rr")
fileUrl <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "data.zip")
unzip("data.zip", exdir = ".")
data <- read.csv("activity.csv", header = TRUE, colClasses = c("numeric", "character", "integer"))
file.remove("data.zip")
data$date <- ymd(data$date)
```

Uploading of required packages, creating and setting the appropriate directory, downloading and uploading of the data are made above. Besides, the date format is changed in order to facilitate the way to deal with dates in our project.

To answer the first question, the important point to state is that the data contains some missing values and they are ignored at this step of analysis. The data is grouped with respect to days and summarized in a way it shows the total number of steps taken each day.

```{r, echo = TRUE}
sum_data <- group_by(data, date) %>%
        summarize(Total_Number = sum(steps)) %>%
        as.data.frame()
```

Then, the code below makes the histogram of total number of steps taken each day:

```{r plot1, echo = TRUE, fig.width = 8}
g1 <- ggplot(sum_data, aes(x = Total_Number))
g1 <- g1 + geom_histogram(color = "black", fill = "#FF9999", binwidth = 1250)
g1 <- g1 + labs(title = "Histogram of Total Number of steps taken each day")
g1 <- g1 + theme(text = element_text(size = 15, face = "italic"), axis.title = element_text(size = 12))
g1 <- g1 + labs(x = "Total number of steps", y = "Frequency")
g1 <- g1 + theme_bw()
print(g1)
```

Adjusting for acceptable binwidth, we can see that 10000 steps occures most frequently. What this leads to is that in most of the days, the steps taken are approximately in 10000 - 11000 interval. In addition, the shape of histogram implies that the total numbers of steps are normally distributed.

Then, the codes provided below report mean and median for total number of steps taken each day.

```{r, echo = TRUE}
mean(select(sum_data, Total_Number)[,1], na.rm = T)
median(select(sum_data, Total_Number)[,1], na.rm = T)
```

Apparently, as our histogram showed us, in most of the days, the steps taken are indeed in 10000-11000 interval. The mean of 10766 firmly stands for the given hypothesis. The median is not so far from the mean. This might be concluded from the rule of thumb of normal distribution. That is, if the data is normally distributed(approximately normally distributed), the mean is equal(approximately equal) to median and equal(approximately equal) to mode, which is quite obvious from the given output.

Answering the second question, the daily activity pattern is concerned. In other words, we want to observe what happens to the steps taken every 5 minute interval. To prepare the data, the important step is to group it by interval and summarize with repspect to average of steps taken in a respective interval.

```{r, echo = TRUE}
interval_data <- group_by(data, interval) %>%
        summarize(mean = mean(steps, na.rm = TRUE)) %>%
        as.data.frame()
```

The best way to observe the pattern is to plot the time-series plot, which is done with the help of below codes.

```{r, echo = TRUE, fig.width = 8}
g2 <- ggplot(interval_data, aes(x = interval, y = mean, group = 1))
g2 <- g2 + geom_point(size = 3, alpha = 2/3, color = "red") + geom_line(color = "black")
g2 <- g2 + scale_x_discrete(breaks = c(0, 500, 1000, 1500, 2000), 
                            labels = c("00:00", "05:00", "10:00", "15:00", "20:00"))
g2 <- g2 + labs(x = "Interval", y = "Averages")
g2 <- g2 + ggtitle("Averages of steps taken for every 5-munite interval across all days")
g2 <- g2 + theme(text = element_text(size = 15, face = "italic"), axis.title = element_text(size = 12))
g2 <- g2 + theme_bw()
g2
```

From the plot above, we can see that the biggest average of steps taken accross all days is in the 07:00 - 09:00 o`clock interval. To be precise, the following shows the exact interval which has the maximum average of steps taken across all days.

```{r, echo = TRUE}
max_steps_inteval <- interval_data[which(max(select(interval_data, mean)) == select(interval_data, mean)),1]
max_steps_inteval
```

So, 835th interval has the maximum average of steps taken. In other words, 8:35 - 8:40 time interval has the maximum average of steps taken across all days. We can assume, that this interval indicates the time when everyone goes to school, university or work and having the maximum average steps is quite reasonable.

Next part of the project contains imputation of missing values, which is the core problem in data analysis. Analysts happen to face this problem a lot and handling it is a bit tricky. For the sake of simplicity, we are going to impute using mean-imputation technique which is quite applicable when facing missingness. The idea is that we choose the average steps taken in each interval and substitute the missing values with them corresponding to the similar interval.
The below code makes the procedure described above:

```{r}
new_data <- data

new_data_interval <- group_by(new_data, interval) %>%
        summarize(mean = mean(steps, na.rm = T)) %>%
        as.data.frame()

index <- apply(is.na(data), 2, which)
index <- index$steps
for(i in 1:length(index)){
        new_data[index[i],"steps"] <- new_data_interval[new_data[index[i],"interval"] 
                                                        == new_data_interval$interval,]$mean
}
```

Firstly, we defined new data set, grouped it by intervals and summarised with respect to means of steps taken. Then, we withdrew the indeces of missing values and assigned means for a given interval accross all days to a particular index of the data. In the end, we obtain new data set with imputed values.

```{r, fig.width = 8}
new_data_date <- group_by(new_data, date) %>%
        summarize(Total_Number = sum(steps)) %>%
        as.data.frame()

g3 <- ggplot(new_data_date, aes(x = Total_Number))
g3 <- g3 + geom_histogram(color = "black", fill = "#FF9999", binwidth = 1250)
g3 <- g3 + labs(title = "Histogram of total number of steps taken each day")
g3 <- g3 + theme(text = element_text(size = 15, face = "italic"), axis.title = element_text(size = 12))
g3 <- g3 + labs(x = "Total number of steps", y = "Frequency")
g3 <- g3 + theme_bw()
g3

mean(select(new_data_date, Total_Number)[,1], na.rm = T)
median(select(new_data_date, Total_Number)[,1], na.rm = T)
```

Looking at the histogram and mean and median of total number of steps taken, astute one might notice that missing value imputation created an artificial band around the mean. This is, indeed, logical because you are imputing the same value and the result is fully justified. The mean and median are equal, which also states for the fact that there is similarity around the mean.

And, finally, the last question of interest is related with distinguishing patterns in weekdays and weekends. Before proceeding to the answer of the question, some pre-processing steps are undertaken. They are as follows:

```{r}
new_data$type <- as.character(wday(new_data$date, label = TRUE))
ind_sat <- which(new_data$type == "Sat")
ind_sun <- which(new_data$type == "Sun")

new_data[c(ind_sat, ind_sun), "type"] <- "Weekend"
new_data[-c(ind_sat, ind_sun), "type"] <- "Weekday"
```

Firstly, labels for each day are defined. Then, using them, we figure out weekends and weekdays. Next, the codes below provide us with the two time-series plots separated with respect to weekdays and weekends.

```{r, fig.width = 10}
new_data_interval_type <- group_by(new_data, interval, type) %>%
        summarize(Average = mean(steps)) %>%
        as.data.frame()

g4 <- ggplot(new_data_interval_type, aes(x = interval, y = Average, group = 1))
g4 <- g4 + geom_line(color = "black") + facet_grid(facet = . ~ type)
g4 <- g4 + geom_point(size = 2, alpha = 1/6, color = "red")
g4 <- g4 + labs(title = "Average steps on weekdays and weekends \n with respect to Time Intervals", 
                x = "Interval", y = "Average")
g4 <- g4 + theme_bw()
g4 <- g4 + scale_x_discrete(breaks = c(0, 500, 1000, 1500, 2000), 
                            labels = c("00:00", "05:00", "10:00", "15:00", "20:00"))
g4
```

Apparently, the plots above show that on weekdays, people are walking much more in the interval between 08:00-09:00. As we have already assummed, the plausible reason for this is that they go to schools, universities or works and therefore, the increase in number of steps taken is again sensible.

# Conclusion

The project is elaborated on data collected by anonymous person and describing the personal movement activity. The questions of interest posed in the beginning of the project are answered in a consistent manner using plotting and computational techniques in R programming language. The first question is to define the mean total number of steps taken per day and it is found to be in 10000 - 11000 interval. To be precise, the mean is equal to 10766 steps taken per day. Secondly, the average daily activity pattern is concerned and it is found that the maximum steps taken in a day is in 08:35 - 08:40 interval, which constitutes the time when the maximum movement activity is generally noticed(going to work, university, school). Thirdly, due to excessive appearance of missing values, imputation is highly recommended and it is done using mean imputation technique. The means of steps for each 5 minute interval is figured out and substituted in a places where missing values occur. After imputation, the difference between total number of steps with and without missing values is to be observed and it is found that imputing missing values evolved into another significantly important problem, generally called as artificial band. Since a lot of values are missing, imputing the same values created a band that is bound around the mean of total number of steps. And the last question is to define the difference between activity patterns on weekdays and weekends. The answer is found to be very interesting. The time-series plots show that on weekdays, the most active time intervals are around 08:00-09:00 and it is substantially different from other intervals, however, on weekends, despite being again the most active interval, the difference with other intervals is not very significant. This makes us conclude that on weekdays people are taking much more steps in comparison with weekends.

