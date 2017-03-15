# 1: Create a function (named readStates) to read a CSV file into R
#1.	Note that you are to read a URL, not a file local to your computer.
#2.	The file is a dataset on state populations (within the United States).
#The URL is: 
# http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv

#Hint: google "read.csv" and "url" with respect to R commands
urlToRead <- "http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
testFrame <- read.csv(url(urlToRead))

readStates <- function(){
    return(testFrame)
    }

readStates()


#Step 2: Clean the dataframe
#3.	Note the issues that need to be fixed (removing columns, removing rows, changing column names).
#4.	Within your function, make sure there are 51 rows (one per state + the district of Columbia). Make sure there are only 5 columns with the columns having the following names (stateName, Jul2010,  Jul2011, base2010, base2011).
#5.	Make sure the last four columns are numbers (i.e. not strings).
testFrame<-testFrame[-1:-8,]

testFrame<-testFrame[,1:5]

testFrame<-testFrame[-52:-58,]

#rename columns 
colnames(testFrame)
cnames <- colnames(testFrame)

cnames[1] <- "stateName"
cnames[2] <- "Jul2010"
cnames[3] <- "Jul2011"
cnames[4] <- "base2010"
cnames[5] <- "base2011"

#display new column names
cnames

colnames(testFrame) <- cnames
colnames(testFrame)

# remove dots and commas
testFrame$stateName <- gsub("\\.","", testFrame$stateName)
testFrame$Jul2010 <- gsub(",", "", testFrame$Jul2010)
testFrame$Jul2011 <- gsub(",", "", testFrame$Jul2011)
testFrame$base2010 <- gsub(",", "", testFrame$base2010)
testFrame$base2011 <- gsub(",", "", testFrame$base2011)

# remove spaces and convert to numeric type
testFrame$Jul2010 <- as.numeric(gsub(" ", "", testFrame$Jul2010))
testFrame$Jul2011 <- as.numeric(gsub(" ", "", testFrame$Jul2011))
testFrame$base2010 <- as.numeric(gsub(" ", "", testFrame$base2010))
testFrame$base2011 <- as.numeric(gsub(" ", "", testFrame$base2011))

#display cleaned table
testFrame


#Step 3: Store and Explore the dataset
#6.	Store  the dataset into a dataframe, called dfStates.
dfStates <- testFrame

#7.	Test your dataframe by calculating the mean for the July2011 data, by doing:
#  mean(dfStates$Jul2011)
# ??? you should get an answer of  6,053,834
mean(dfStates$Jul2011)

#Step 4:  Find the state with the Highest Population 
#8.	Based on the July2011 data, what is the population of the state with the highest population?
sortedStates <- dfStates[order(-dfStates$Jul2011),]
head(sortedStates,1)
sortedStates[1,3]

#   What is the name of that state?
sortedStates[1,1]

#9.	Sort the data, in increasing order, based on the July2011 data. 
sortedStates <- dfStates[order(dfStates$Jul2011),]


#Step 5:  Explore the distribution of the states
#10.	Write a function that takes two parameters. The first is a vector and the second is a number.
#11.	The function will return the percentage of the elements within the vector that is less than the same (i.e. the cumulative distribution below the value provided).
#12.	For example, if the vector had 5 elements (1,2,3,4,5), with 2 being the number passed into the function, the function would return 0.2 (since 20% of the numbers were below 2).
#13.	Test the function with the vector 'dfStates$Jul2011Num', and the mean of dfStates$Jul2011Num'.
#There are many ways to write this function (described in #10 above) - so please try to write multiple versions of this function - which do you think is best?

#Generating the function
PElement <- function(vectorex,input){
  x <- sum(vectorex < input)
  y <- length(vectorex)
  percentage <- x/y
  return(percentage)
}

#Testing 
PElement(dfStates$Jul2011,1000000)





    