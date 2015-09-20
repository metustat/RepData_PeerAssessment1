###Loading and pre-processing data#
#Uploading required packages#
library(lubridate)
library(dplyr)
library(ggplot2)
##
#Downloading and uploading data into R#
dir.create("project_rr")
setwd("./project_rr")
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "data.zip")
unzip("data.zip", exdir = ".")
data <- read.csv("activity.csv", header = TRUE, colClasses = c("numeric", "character", "integer"))
file.remove("data.zip")
head(data)
str(data)
#

#Transformation and formatting#
data$date <- ymd(data$date)
#

# 1) What is mean total number of steps taken per day?
sum_data <- group_by(data, date) %>%
        summarize(Total_Number = sum(steps)) %>%
        as.data.frame()

png("plot1.png", width = 480, height = 480)
g1 <- ggplot(sum_data, aes(x = Total_Number))
g1 <- g1 + geom_histogram(color = "black", fill = "#FF9999", binwidth = 1250)
g1 <- g1 + labs(title = "Histogram of Total Number of steps taken each day")
g1 <- g1 + theme(text = element_text(size = 15, face = "italic"), axis.title = element_text(size = 12))
g1 <- g1 + labs(x = "Total number of steps", y = "Frequency")
g1 <- g1 + theme_bw()
g1
dev.off()

mean(select(sum_data, Total_Number)[,1], na.rm = T)
median(select(sum_data, Total_Number)[,1], na.rm = T)# The median and mean are quite similar to each other meaning that Total Number of steps taken each day are normally distributed. The same implication is derived from the aforementioned histogram.
#

# 2) What is the average daily activity pattern?
interval_data <- group_by(data, interval) %>%
        summarize(mean = mean(steps, na.rm = TRUE)) %>%
        as.data.frame()

png("plot2.png", width = 480, height = 480)
g2 <- ggplot(interval_data, aes(x = interval, y = mean, group = 1))
g2 <- g2 + geom_point(size = 3, alpha = 2/3, color = "red") + geom_line(color = "black")
g2 <- g2 + scale_x_discrete(breaks = c(0, 500, 1000, 1500, 2000), 
                            labels = c("00:00", "05:00", "10:00", "15:00", "20:00"))
g2 <- g2 + labs(x = "Interval", y = "Averages")
g2 <- g2 + ggtitle("Averages of steps taken for every 5-munite interval across all days")
g2 <- g2 + theme(text = element_text(size = 15, face = "italic"), axis.title = element_text(size = 12))
g2 <- g2 + theme_bw()
g2
dev.off()

max_steps_inteval <- interval_data[which(max(select(interval_data, mean)) == select(interval_data, mean)),1]
max_steps_inteval
#

# 3) Imputing missing values
apply(is.na(data), 2, sum)

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
new_data_date <- group_by(new_data, date) %>%
        summarize(Total_Number = sum(steps)) %>%
        as.data.frame()

png("plot3.png", width = 480, height = 480)
g3 <- ggplot(new_data_date, aes(x = Total_Number))
g3 <- g3 + geom_histogram(color = "black", fill = "#FF9999", binwidth = 1250)
g3 <- g3 + labs(title = "Histogram of total number of steps taken each day")
g3 <- g3 + theme(text = element_text(size = 15, face = "italic"), axis.title = element_text(size = 12))
g3 <- g3 + labs(x = "Total number of steps", y = "Frequency")
g3 <- g3 + theme_bw()
g3
dev.off()

mean(select(new_data_date, Total_Number)[,1], na.rm = T)
median(select(new_data_date, Total_Number)[,1], na.rm = T)
#

# 4) Are there differences in activity patterns between weekdays and weekends?

new_data$type <- as.character(wday(new_data$date, label = TRUE))
ind_sat <- which(new_data$type == "Sat")
ind_sun <- which(new_data$type == "Sun")

new_data[c(ind_sat, ind_sun), "type"] <- "Weekend"
new_data[-c(ind_sat, ind_sun), "type"] <- "Weekday"


new_data_interval_type <- group_by(new_data, interval, type) %>%
        summarize(Average = mean(steps)) %>%
        as.data.frame()

png("plot4.png", width = 480, height = 480)
g4 <- ggplot(new_data_interval_type, aes(x = interval, y = Average, group = 1))
g4 <- g4 + geom_line(color = "black") + facet_grid(facet = . ~ type)
g4 <- g4 + geom_point(size = 2, alpha = 1/6, color = "red")
g4 <- g4 + labs(title = "Average steps on weekdays and weekends \n with respect to Time Intervals", 
                x = "Interval", y = "Average")
g4 <- g4 + theme_bw()
g4 <- g4 + scale_x_discrete(breaks = c(0, 500, 1000, 1500, 2000), 
                            labels = c("00:00", "05:00", "10:00", "15:00", "20:00"))
g4
dev.off()


#
