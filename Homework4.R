#IST 687
#Yuhui Zhong - Lab Section Thus.12:30-13:50
#Homework 2

#Step 1: Write a summarizing function to understand the distribution of a vector
#1.	The function, call it 'printVecInfo' should take a vector as input
#2.	The function should print the following information:
#   a.	Mean
#   b.	Median
#   c.	Min & max
#   d.	Standard deviation
#   e.	Quantiles (at 0.05 and 0.95)
#   f.	Skewness
#Note for skewness, you can use the function in the 'moments' library.


#install 'moments' package
install.packages("moments")

#import 'moments' package
library(moments)

#create the function
printVecInfo <- function(a){
  cat(" Mean: ", mean(a), "\n",
      "Median: ", median(a), "\n",
      "Min: ", min(a), "\n",
      "Max: ", max(a), "\n",
      "Standard Deviation: ", sd(a), "\n",
      "Quantile (0.05 - 0.95): ",quantile(a,c(0.05,0.95)), "\n",
      "Skewness: ", skewness(a), "\n"
      )
  }


#3.	Test the function with a vector that has (1,2,3,4,5,6,7,8,9,10,50). You should see something such as:
#[1] "mean: 9.54545454545454"
#[1] "median: 6"
#[1] "min: 1  max: 50"
#[1] "sd: 13.7212509368762"
#[1] "quantile (0.05 - 0.95): 1.5 -- 30"
#[1] "skewness: 2.62039633563579"

#create a test vector
vtest <- c(1,2,3,4,5,6,7,8,9,10,50)

#test the function
printVecInfo(vtest)



#Step 2: Creating Samples in a Jar
#4.	Create a variable 'jar' that has 50 red and 50 blue marbles
rm <- "red marble"
bm <- "blue marble"

v.rm <- replicate(50,rm)
v.bm <- replicate(50,bm)

jar <- replicate(100, c(v.rm,v.bm))
# jar <- c(replicate(50,"red"), replicate(50,"green"))

# (hint: the jar can have strings as objects, with some of the strings being 'red' and some of the strings being 'blue'
#5.	Confirm there are 50 reds by summing the samples that are red
sum(jar == "red marble")


#6.	Sample 10 'marbles' (really strings) from the jar. How many are red? 
table(sample(jar,10,replace = TRUE))["red marble"]

#What was the percentage of red marbles?
table(sample(jar,10,replace = TRUE))["red marble"]/10

#7.	Do the sampling 20 times, using the 'replicate' command. 
#This should generate a list of 20 numbers. 
#Each number is the mean of how many reds there were in 10 samples. 
#Use your printVecInfo to see information of the samples. Also generate a histogram of the samples.

replicate(20,mean(table(sample(jar,10,replace = TRUE))["red marble"]))
hist(replicate(20,mean(table(sample(jar,10,replace = TRUE))["red marble"])))
printVecInfo(replicate(20,mean(table(sample(jar,10,replace = TRUE))["red marble"])))

#8.	Repeat #7, but this time, sample the jar 100 times. 
#You should get 100 numbers, this time each number represents the mean of how many reds there were in the 100 samples. 
#Use your printVecInfo to see information of the samples. Also generate a histogram of the samples.

replicate(20,mean(table(sample(jar,100,replace = TRUE))["red marble"]))
hist(replicate(20,mean(table(sample(jar,100,replace = TRUE))["red marble"])))
printVecInfo(replicate(20,mean(table(sample(jar,100,replace = TRUE))["red marble"])))

#9.	Repeat #8, but this time, replicate the sampling 1000 times. 
#You should get 1000 numbers, this time each number represents the mean of how many reds there were in the 1000 samples. 
#Use your printVecInfo to see information of the samples. Also generate a histogram of the samples.

replicate(1000,mean(table(sample(jar,100,replace = TRUE))["red marble"]))
hist(replicate(1000,mean(table(sample(jar,100,replace = TRUE))["red marble"])))
printVecInfo(replicate(1000,mean(table(sample(jar,100,replace = TRUE))["red marble"])))

#Step 3: Explore the airquality dataset
#10.	Store the 'airquality' dataset into a temporary variable
#11.	Clean the dataset (i.e. remove the NAs)
#12.	Explore Ozone, Wind and Temp by doing a 'printVecInfo' on each as well as generating a histogram for each

airtest <- airquality
airtest <- na.omit(airtest)

#printVecInfo & hist functions for Ozone

printVecInfo(airtest$Ozone)
hist(airtest$Ozone)

#printVecInfo & hist functions for Wind

printVecInfo(airtest$Wind)
hist(airtest$Wind)

#printVecInfo & hist functions for Temp

printVecInfo(airtest$Temp)
hist(airtest$Temp)
  