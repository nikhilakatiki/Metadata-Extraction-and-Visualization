#Load the package required to read JSON files.
install.packages("sqldf")
install.packages("na.tools")
library (sqldf)
library(jsonlite)
library(tidyverse)
library(stringi)
library(na.tools)

# Reading the JSON file, saving it to data and writing JSON to CSV
data<- fromJSON("ait582-proj-data.json")
write.csv(data, file = "AIT582.csv")
CSV <- read.csv("AIT582.csv")
View(CSV)
CSV$X <- NULL
dim(CSV)

#Sorting the column names as per requirement
FlightsData <- CSV[, c(6,3,2,4,5,1)]
view(FlightsData)

#Creating a new column for GENDER and classifying genders
G1 <- sub(".*,","",FlightsData$DESCRIPTION)
G2 <- sub("(\\.).*","",G1)
rename <- stri_replace_all_regex(G2, pattern = c("\\bMr\\b","\\bMaster\\b","\\bMrs\\b","\\bMiss\\b"), 
                                 replacement = c("Male","Male","Female","Female"), 
                                 vectorize_all = FALSE,mode = c("all"))
FlightsData$GENDER= rename

#Creating new column for AGE
AGE <- sub(".*;","",FlightsData$DESCRIPTION)
FlightsData$AGE= as.numeric(AGE)
View(FlightsData)

#Metadata Extraction
#sql query in order to find out those customers who doesnt have either male or female as thier GENDER
unknown <- sqldf( "select * from FlightsData where GENDER not like '%Male%'")
#We can see there are total of 26 customers who doesnt have a GENDER specified.Using dbpedia, we have found out the GENDER for each and are assigned.
unknown$GENDER='Male'
unknown$GENDER[c(9,12,16,22,24)]='Female'
Airlines <- sqldf( "select * from FlightsData where GENDER like '%Male%' union select * from unknown")
view(Airlines)

#Data cleaning- finding out missing values
sapply(Airlines, function(x) sum(is.na(x)))

#we see that there are 177 missing vlaues for Age column: Replacing null values of AGE with the mean
summary(Airlines)
str(Airlines)
dim(Airlines)

#we see that mean is 29.70, we are rounding to 30 and we are replacing the missing values with mean
Airlines$AGE <- ceiling(na.mean(Airlines$AGE, option = "mean"))

#Finding out customers whos fare is zero and replacing with mean
unknownfare <- sqldf( "select * from FlightsData where FARE == 0.0000 ")
mean(Airlines$FARE)
Airlines$FARE <- as.numeric(Airlines$FARE)
Airlines$FARE[Airlines$FARE == 0.0000 ] <- 32.2042
View(Airlines)

zeroFare <- sqldf( "select * from Airlines where FARE == 0.0000 ")
print(zeroFare)

#Imputing the values for age
unknownAge <- sqldf( "select * from Airlines where AGE <= 12 and description like '%Mr%'")
unknownAge$AGE=30

write.csv(Airlines,"downloads\\Airlines.csv")

#Correlation
install.packages("corrplot")
install.packages("corrgram")
library(corrgram)
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corrgram(Airlines), method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE )


my_cols <- c("#00AFBB", "#EE9988", "#FC4E07") 
pairs(Airlines[, c(1,2,4,5,6,8)], pch = 19,lower.panel = NULL,cex = 0.5,col = my_cols[Airlines$SEATCLASS])


install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
my_data <- Airlines[, c(1,2,4,5,6,8)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

