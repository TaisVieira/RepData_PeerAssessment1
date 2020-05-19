library(dplyr)
library(lattice)

#Loading and preprocessing the data
data<-read.csv("activity.csv")
data$date<-as.Date(data$date, format="%Y-%m-%d")


#What is mean total number of steps taken per day?
#Make a histogram and report the mean and median of the total number of steps taken per day
grouped_day<-group_by(data, data$date)
day_total<-summarise(grouped_day, "sum"=sum(steps, na.rm=T))
hist(day_total$sum, breaks=35, main="Total number of steps taken per day", xlab="Total number of steps", col="gray")
abline(v=mean(day_total$sum), col="red", lwd=2)
abline(v=median(day_total$sum), col="blue", lwd=2)
legend("topright", c("mean: 9354", "median: 10395"), col=c("red", "blue"), lwd=2)


#What is the average daily activity pattern?
#Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days. 
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
grouped_interval<-group_by(data, data$interval)
interval_avg<-summarise(grouped_interval, "mean"=mean(steps, na.rm=T))
plot(interval_avg, type="l", main="Averge number of steps taken per interval", xlab="Interval (minutes)", ylab="Number of steps")
points(x=interval_avg[interval_avg$mean==max(interval_avg$mean),1], y=max(interval_avg$mean), pch=19, col="red")
text(x=1200, y=190, labels="Maximun average number \nof steps (206 steps) \nat the 288th interval \n(835 to 840 minutes)", cex = 0.8)


#Imputing missing values
#Calculate and report the total number of missing values in the dataset
sum(is.na(data$steps))

#Create a new dataset that is equal to the original dataset but with the missing data filled in. Make a histogram of the total 
#number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these 
#values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the 
#estimates of the total daily number of steps?
interval_median<-summarise(grouped_interval, "median"=median(steps, na.rm=T))   #create a data frame with the medians for each interval
data_replaced<-merge(data, interval_median, by.x="interval", by.y="data$interval")   #merge it with original data frame by the interval
data_replaced$steps[is.na(data_replaced$steps)]<-data_replaced$median[is.na(data_replaced$steps)]    #replace NA's with median for that interval

grouped_day_replaced<-group_by(data_replaced, data_replaced$date)
day_total_replaced<-summarise(grouped_day_replaced, "sum"=sum(steps))

hist(day_total_replaced$sum, breaks=35, main="Total number of steps taken per day", xlab="Total number of steps", col="gray")
abline(v=mean(day_total_replaced$sum), col="red", lwd=2)
abline(v=median(day_total_replaced$sum), col="blue", lwd=2)
legend("topright", c("mean: 9504", "median: 10395"), col=c("red", "blue"), lwd=2)   #the mean changed slightly, the median remained the same


#Are there differences in activity patterns between weekdays and weekends?
data_replaced$weekday<-ifelse(!(data_replaced$weekday %in% c("sábado", "domingo")), "weekday", data_replaced$weekday)
data_replaced$weekday<-ifelse(data_replaced$weekday %in% c("sábado", "domingo"), "weekend", data_replaced$weekday)

xyplot(steps~interval|weekday, group=weekday, data=data_replaced, type="l")












