#IST 687
#Yuhui Zhong - Lab Section Thus.12:30-13:50
#Homework 6

#import ggplot2 & reshape
install.packages('ggplot2')
install.packages("reshape")
library(ggplot2)
library(reshape)
#Step 1: Load the data
#We will use the airquality data set, that you should already have as part of your R installation.
airquality

#Step 2: Clean the data
#After you load the data, there will be some NAs in the data. You need to figure out what to do about those nasty NAs.
#load data to a variable
aq <- airquality

#replace NAs with mean value of this column
aq$Ozone[is.na(aq$Ozone)] <- mean(aq$Ozone, na.rm=TRUE) 
aq$Solar.R[is.na(aq$Solar.R)] <- mean(aq$Solar.R, na.rm=TRUE) 

#Step 3: Understand the data distribution
#Create the following visualizations:
#.	Histograms for each of the variables
hist(aq$Ozone)
hist(aq$Solar.R)
hist(aq$Wind)
hist(aq$Temp)
hist(aq$Month)
hist(aq$Day)

#.	Boxplot for Ozone, and boxplots for different wind values (round the wind to get a good number of "buckets"
ggplot(aq, aes(x = factor(0), y = Ozone)) + geom_boxplot()
ggplot(aq, aes(x = factor(0), y = Wind, group = Wind)) + geom_boxplot(color = "blue", size=2)
                                                             
#Step 3: Explore how the data changes over time
#First, make sure to create appropriate dates (this data was from 1973). 
aq$Date <- as.Date(paste("1973", airquality$Month, airquality$Day, sep = "-"))
#Then create line charts for ozone, temp, wind and solar.R
ggplot(aq, aes(x = Date, y = Ozone)) + geom_line(color = "blue", size = 2)
ggplot(aq, aes(x = Date, y = Temp)) + geom_line(color = "red", size = 2)
ggplot(aq, aes(x = Date, y = Wind)) + geom_line(color = "green", size = 2)
ggplot(aq, aes(x = Date, y = Solar.R)) + geom_line(color = "yellow", size = 2)

#Creating scale function for effectively drawing four lines in one chart
scale <- function(v){
  min <- min(v)
  max <- max(v)
  v1 <- (v-min)/(max-min)
  return (v1)
}

#scale first four cols into a vector called saq
saq <- sapply(aq[,1:4], scale)

#unlist the date col
date <- unlist(aq$Date)

#combine saq and date into a new datafream
saqdf <- data.frame(saq, date)

#melt the data with id = col date
saqdfm <- melt(saqdf, id=c("date"))
saqdfm

#(one line chart for each, and then one chart with 4 lines, each having a different color). 
ggplot(saqdfm, aes(x=date, y=value, color=variable, group=variable)) + geom_line(size = 2) + ggtitle('LineChart for AirQuality')
#Note that for the chart with 4 lines, you need to think about how to effectively use the y-axis.
                                                             
#Step 4: Look at all the data via a Heatmap
#Create a heatmap, with each day along the x-axis and ozone, temp, wind and solar.r along the y-axis, and days as rows along the y-axis. 
ggplot(saqdfm, aes(x=date, y=variable)) + geom_tile(aes(fill=saqdfm$value)) + scale_fill_gradient(low='white',high='red') + ggtitle('Heatmap for AirQuality')
#Note that you need to figure out how to show the relative change equally across all the variables.
ggplot(saqdfm, aes(x=variable, y=date)) + geom_tile(aes(fill=saqdfm$value)) + scale_fill_gradient(low='white',high='red') + ggtitle('Heatmap for AirQuality')

                                                             
#Step 5: Look at all the data via a scatter chart
#Create a scatter chart, with the x-axis representing the wind, 
#the y-axis representing the temperature, 
#the size of each dot representing the ozone and the color representing the solar.R
ggplot(saqdf, aes(x=Wind, y=Temp)) + geom_point(aes(size=Ozone, color=Solar.R))

                                                             
#Step 6: Final Analysis
#.	Do you see any patterns after exploring the data?  
#Yes, color and chart type have great impact on the results.
#.	What was the most useful visualization?
#In my point of view, the heat map works best for now.                                                             