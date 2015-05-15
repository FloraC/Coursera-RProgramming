## Quiz1 Review

## verctor addition
x1<-1:4
y1<-2:3
z1<-2
Ans1_1<-x1+y1
Ans1_1
class(Ans1_1)
Ans1_2<-x1+z1
Ans1_2
class(Ans1_2)
##Suppose I have a vector x <- c(3, 5, 1, 10, 12, 6) and I want to set all elements of this vector that are less than 6 to be equal to zero. What R code achieves this?
##You can create a logical vector with the expression x %in% 1:5 and then use the [ operator to subset the original vector x.
x[x %in% 1:5] <- 0
x

##unpack the data package
Q1data<- read.csv(unzip("rprog-data-quiz1_data.zip"))
## what are the column names of the dataset?
names(Q1data)
## Extract the first 2 rows of the data frame and print them to the console.
Q1data[1:2,]
## How many observations (i.e. rows) are in this data frame?
nr<-nrow(Q1data)
nr
##Extract the last 2 rows of the data frame and print them to the console.
##The `tail()' function is an easy way to extract the last few elements of an R object.
tail(Q1data,2)
Q1data[(nr-1):nr,]
##What is the value of Ozone in the 47th row?
OR47<-Q1data[["Ozone"]][47]
OR47
##How many missing values are in the Ozone column of this data frame?
cOzone<-Q1data[["Ozone"]]
sum(is.na(cOzone))
##What is the mean of the Ozone column in this dataset? Exclude missing values (coded as NA) from this calculation.
mean(cOzone[!is.na(cOzone)])
##Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?
cSolar<-Q1data[["Solar.R"]]
cSolarCal<-cSolar[(cOzone>31)&(Q1data[["Temp"]]>90)]
mean(cSolarCal[!is.na(cSolarCal)])
##What is the mean of "Temp" when "Month" is equal to 6?
cTemp<-Q1data[["Temp"]]
cMonth<-Q1data[["Month"]]
cTempCal<-cTemp[cMonth==6]
mean(cTempCal[!is.na(cTempCal)])
##What was the maximum ozone value in the month of May (i.e. Month = 5)?
cOzoneCal<-cOzone[cMonth==5]
max(cOzoneCal[!is.na(cOzoneCal)])