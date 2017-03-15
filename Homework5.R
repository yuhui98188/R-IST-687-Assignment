#Step 1: Load the data
#Read in the following JSON dataset
#https://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD

#install and import packages
install.packages("RCurl")
install.packages("RJSONIO")
library(RCurl)
library(RJSONIO)

#store data into a vector and translate into JSON format
results <- fromJSON("https://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD", nullValue = NA)

#test the length of the data
length(results)


accident <- results[[2]]


#Step 2: Clean the data
#After you load the data, remove the first 8 columns, and then, to make it easier to work with, name the rest of the columns as follows:
#Note, not surprisingly, it is in JSON format.  You should be able to see that the first result is the metadata (information about the data) and the second is the actual data.
#namesOfColumns <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2")

#accident <- sapply(accident, function(x) ifelse(x == "NULL", NA, x))

numRows <- length(accident)

#transit data from list format into table
dfResults <- data.frame(matrix
                        (unlist(accident), 
                          nrow = numRows, byrow = T),
                        stringsAsFactors = FALSE
                        )

#dfResults <- sapply(dfResults, function(x) ifelse(x == "NULL", NA, x))

#remove first 8 columns
dfResults <- dfResults[,-1:-8]

#store new column names in to a vector
nameList <- c("CASE_NUMBER","BARRACK",
              "ACC_DATE","ACC_TIME",
              "ACC_TIME_CODE","DAY_OF_WEEK",
              "ROAD","INTERSECT_ROAD",
              "DIST_FROM_INTERSECT","DIST_DIRECTION",
              "CITY_NAME","COUNTY_CODE","COUNTY_NAME",
              "VEHICLE_COUNT","PROP_DEST",
              "INJURY","COLLISION_WITH_1",
              "COLLISION_WITH_2")


#rename column names
names(dfResults) <- nameList

#clear up "," and space in data by creating a Numberize function
Numberize <- function(inputVector)
{
  inputVector<-gsub(",","", inputVector)
  inputVector<-gsub(" ","", inputVector)
  
  return(as.numeric(inputVector))
}

Numberize(dfResults)

#Step 3: Understand the data using SQL (via SQLDF)
#IMPORT SQLDF
install.packages('sqldf')
library(sqldf)


#Answer the following questions:
#.	How many accidents happen on SUNDAY  
sqldf("SELECT COUNT(DAY_OF_WEEK) AS NUMBER_OF_ACCIDENTS_HAPPENED_ON_SUNDAY
       FROM dfResults
       WHERE DAY_OF_WEEK LIKE '%SUNDAY%'
      ")
#.	How many accidents had injuries (might need to remove NAs from the data)
sqldf("SELECT COUNT(INJURY) AS NUMBER_OF_INJURY
       FROM dfResults
       WHERE INJURY = 'YES'
      ")
#.	List the injuries by day
sqldf("SELECT 
       DAY_OF_WEEK,
       COUNT(INJURY) AS NUMBER_OF_INJURY
       FROM dfResults
       WHERE INJURY = 'YES'
       GROUP BY DAY_OF_WEEK
      ")


#Step 4: Understand the data using tapply
#Answer the following questions (same as before) - compare results:
#.	How many accidents happen on Sunday
tapply(dfResults$DAY_OF_WEEK,dfResults$DAY_OF_WEEK,length)

#.	How many accidents had injuries (might need to remove NAs from the data)
tapply(dfResults$INJURY,dfResults$INJURY,length)

#.	List the injuries by day
tapply(dfResults$INJURY,dfResults$DAY_OF_WEEK,function(x) length(which(x == "YES")))
