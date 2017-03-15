#IST 687
#Yuhui Zhong - Lab Section Thus.12:30-13:50
#Homework 7

#Step 1: Load the Data
#1)	Read the data - using the gdata package we have previously used. 
# import and use the gdata package
install.packages('gdata')
library(gdata)
#package gdata doesn't work
#tried codeS below with working directory changed/no change
#incomeDF <- read.xls("/Users/yzhon_000/Downloads/MedianZIP.xlsx")
#incomeDF <- read.xls(perl = "/Users/yzhon_000/Downloads/MedianZIP.xlsx")
#error:
#Error in path.expand(xls) : argument "xls" is missing, with no default
#Error in file.exists(tfn) : invalid 'file' argument

#import xlsx package instead, problem solved
install.packages('xlsx')
library(xlsx)

#change the working dir to file location
setwd("C://Users/yzhon_000/Downloads/")
#ensure the file location
getwd()
#read file save into incomeDF
incomeDF = read.xlsx("MedianZIP.xlsx", sheetName = "nation")

#2)	Clean up the dataframe
#a.	Remove any info at the front of the file that's not needed
#b.	Update the column names (zip, median, mean, population)
#create a vector to same new column names
colName <- c("zip", "median", "mean", "population")
#update column names
names(incomeDF) <- colName
#delete first row as it's duplicated with the column names
incomeDF <- incomeDF[-1,]
#create Numberize function
Numberize <- function(inputVector)
{
  inputVector<-gsub(",","", inputVector)
  inputVector<-gsub(" ","", inputVector)
  
  return(as.numeric(inputVector))
}
#clear up "," and " " in the incomeDF data frame
incomeDF1 <- data.frame(sapply(incomeDF, Numberize))

#notice there are certain data not formatted in number, and replaced by NAs automatically
#find the NAs location
sapply(incomeDF1, function(x) sum(is.na(x)))
#there are 7 NAs located in the 'mean' column
#replacing NAs with data from the same rows in 'median' column where the NAs exist 
incomeDF1[which(is.na(incomeDF1), arr.ind=TRUE)[,c('row')],'mean']<-incomeDF1[which(is.na(incomeDF1), arr.ind=TRUE)[,c('row')],'median']

#3)	Load the 'zipcode' package
install.packages("zipcode")
library(zipcode)
data("zipcode")
#4)	Merge the zip code information from the two data frames (merge into one dataframe)
incomeDF1$zip <- clean.zipcodes(incomeDF1$zip)
incomeDF1

incomeDF2 <- merge(incomeDF1, zipcode, by = "zip")
#5)	Remove Hawaii and Alaska (just focus on the 'lower 48' states)
incomeDF2 <- incomeDF2[incomeDF2$state != "AK", ]
incomeDF2 <- incomeDF2[incomeDF2$state != "HI", ]

#Step 2: Show the income & population per state
#1)	Create a simpler dataframe, with just the average median income and the the population for each state.
income <- tapply(incomeDF2$median, incomeDF2$state, mean)
pop <- tapply(incomeDF2$population, incomeDF2$state, sum)
state <- rownames(income)
incomepop <- data.frame(state,income,pop)

#2)	Add the state abbreviations and the state names as new columns (make sure the state names are all lower case)
incomepop$state_name <- tolower(state.name[match(incomepop$state,state.abb)])

#3)	Show the U.S. map, representing the color with the average median income of that state
install.packages("ggplot2")
library(ggplot2)

us <- map_data("state")
mapIncome <- ggplot(incomepop, aes(map_id = state_name))+geom_map(map = us, aes(fill = incomepop$income))+expand_limits(x = us$long, y = us$lat)+coord_map()+ggtitle("Average Median Income of the U.S")
mapIncome
#4)	Create a second map with color representing the population of the state
mapPop <- ggplot(incomepop, aes(map_id = state_name))+geom_map(map = us, aes(fill = incomepop$pop))+expand_limits(x = us$long, y = us$lat)+coord_map()+ggtitle("Population of the U.S")
mapPop
#Step 3: Show the income per zip code
#1)	 Have draw each zip code on the map,  where the color of the 'dot' is based on the median income. To make the map look appealing, have the background of the map be black.
#adding lower case state name into incomeDF2 dataset
incomeDF2$state_name <- tolower(state.name[match(incomeDF2$state,state.abb)])

mapZip <- ggplot(incomeDF2, aes(map_id = state_name))+geom_map(map = us, fill = "black", color = "white")
mapZip <- mapZip + expand_limits(x = us$long, y = us$lat)+geom_point(data = incomeDF2, aes(x = incomeDF2$longitude, y = incomeDF2$latitude, color = incomeDF2$median))
mapZip <- mapZip + coord_map()+ggtitle("Median Income Based on Zipcode of the U.S")
mapZip
#Step 4: Show Zip Code Density

#1)	Now generate a different map, one where we can easily see where there are lots of zip codes, and where there are few (using the 'stat_density2d' function).
zipDsty <- ggplot(incomeDF2, aes(map_id = state_name, x = incomeDF2$longitude, y = incomeDF2$latitude))+geom_map(map = us, fill = "black", color = "white")
zipDsty <- zipDsty + expand_limits(x = us$long, y = us$lat) + geom_point(color = "gray") + geom_density2d()
zipDsty <- zipDsty + coord_map() + ggtitle("Zip Code Denisty of the U.S")
zipDsty
#Step 5: Zoom in to the region around NYC

#1)	Repeat steps 3 & 4, but have the image / map be of the northeast U.S. (centered around New York).
#install and import ggmap
install.packages("ggmap")
library(ggmap)
#finding lat&lon of NYC, ny
latlon <- geocode("NYC, ny")
#show income in this area
mapZipZoomed <- mapZip + xlim(latlon$lon-1, latlon$lon+1) + ylim(latlon$lat-1,latlon$lat+1) + coord_map()
mapZipZoomed
#show zip code density in this area
zipDstyZoomed <- zipDsty + xlim(latlon$lon-1, latlon$lon+1) + ylim(latlon$lat-1,latlon$lat+1) + coord_map()
zipDstyZoomed
