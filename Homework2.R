#IST 687
#Yuhui Zhong - Lab Section Thus.12:30-13:50
#Homework 2

#Create a new vector for storing data 
mtcars
mt <- mtcars

#Step 1: Which car has the best HP (hp stands for "horse power")  
#1)	Is higher or lower HP best?
# higher
#2)	Which car has the best hp?
row.names(mt[which.max(mt$hp),])

#Step 2: Explore mpg (mpg stands for "miles per gallon")  
#3)	What is the highest mpg?
mt[which.max(mt$mpg),1]

#4)	Which car has the highest mpg?
row.names(mt[which.max(mt$mpg),])

#5)	Create a sorted dataframe, based on mpg
# Sort by ascending 
mt[order(mt$mpg),]
# Sort by decending
mt[order(-mt$mpg),]


#Step 3: Which car has the "best" combination of mpg and hp?
#6)	What logic did you use?
# The important rate between mpg & hp should be equal
#7)	Which car?
# Define the important rate as 0.5
percentMPG <- 0.5
#first for each attribute, scale between 0-1
mpgScaled <- (mt$mpg - min(mt$mpg))/(max(mt$mpg)-min(mt$mpg))

hpScaled <- (mt$hp-min(mt$hp))/(max(mt$hp)-min(mt$hp))

#now combine them
mt$rating <- mpgScaled*percentMPG + hpScaled*(1-percentMPG)

#Which is the highest
top <- which.max(mt$rating)

#print out top
mt[top,]

#create the sorted list (negative sign puts highest first)
mt <- mt[order(-mt$rating),]

#list the top cars - top one is your best 
head(mt)



#Step 4:  Which car has "best" car combination of mpg and hp, where mpg and hp must be given equal weight?
#define a variable that states how much mpg should count (range of 0 to 1)
percentMPG <- 0.5
#first for each attribute, scale between 0-1
mpgScaled <- (mt$mpg - min(mt$mpg))/(max(mt$mpg)-min(mt$mpg))

hpScaled <- (mt$hp-min(mt$hp))/(max(mt$hp)-min(mt$hp))

#now combine them
mt$rating <- mpgScaled*percentMPG + hpScaled*(1-percentMPG)

#Which is the highest
top <- which.max(mt$rating)

#print out top
mt[top,]

#create the sorted list (negative sign puts highest first)
mt <- mt[order(-mt$rating),]

#list the top cars - top one is your best 
head(mt)

