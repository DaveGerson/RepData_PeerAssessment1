wd <- "C:/Users/gerson64/Desktop/Dropbox Sync/Dropbox/Coursera/reproducibleresearch/project1/repdata-data-activity"
setwd(wd)

#Loading and preprocessing the data
dat <- read.csv("activity.csv" , stringsAsFactors = FALSE)


#What is mean total number of steps taken per day?

dayMeans <- aggregate( dat$steps , 
				by=list( dat$date ) ,
				FUN = mean , na.rm = TRUE )
print(paste0("The mean number of steps taken on a daily basis is :", round(mean(dayMeans$x , na.rm = TRUE ), 0) ))
	
#Average daily Activity Pattern
intervalMeans <- aggregate( dat$steps , 
				by=list( dat$interval ) ,
				FUN = mean , na.rm = TRUE )

plot(
intervalMeans$x , 
xlab = "intervals" , 
ylab = "average steps" ,
main = "Average daily Activity Pattern" ,
 type = "l"
)

#Imputing Missing Values

nrow( dat[is.na(dat$steps),]) #2304
nrow( dat[is.na(dat$date),]) #0
nrow( dat[is.na(dat$interval),]) #0

names(intervalMeans)[1]<- "interval"	
dat[is.na(dat$steps),"steps"] <- merge( dat[is.na(dat$steps),] , intervalMeans , by  = "interval")$x

#Are there differences in activity patterns between weekdays and weekends?
weekday <- as.numeric(as.character( as.Date(dat$date ), format = "%w"))			
weekday <- ifelse(weekday == 0 , "Weekend" ,  ifelse(weekday == 6 , "Weekend" , "Weekday")) 

intervalMeans <- aggregate( dat$steps , 
				by=list( dat$interval , weekday ) ,
				FUN = mean , na.rm = TRUE )
names(intervalMeans)[1:2] <- c("interval","dateVal")
intervalMeans[,2] <- as.factor(intervalMeans[,2])

library(lattice)
xyplot( x ~ interval | dateVal , data = intervalMeans,
      type = 'l',
      xlab = 'Interval',
      ylab = 'Steps',
      layout = c(1,2))