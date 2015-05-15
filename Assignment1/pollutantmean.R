## Part1 of Assignment1
## Write a function named 'pollutantmean' that 
## calculates the mean of a pollutant (sulfate or nitrate) 
## across a specified list of monitors. 
## The function 'pollutantmean' takes three arguments: 
## 'directory', 'pollutant', and 'id'. 
## Given a vector monitor ID numbers, 
## 'pollutantmean' reads that monitors' particulate matter data 
## from the directory specified in the 'directory' argument 
## and returns the mean of the pollutant across all of the monitors, 
## ignoring any missing values coded as NA. 
## A prototype of the function is as follows

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  ## load the needed files
  files <- 1:length(id)
  info <- NULL ## store the data need to cal
  calinfo <- NULL
  num <- 1:1
  ## define the file names and read
  for (i in id) {
    if (i <= 9) {
      files[[num]] <- paste("00",i,".csv", sep = "")
      datafile <- read.csv(file.path(".", directory,files[[num]]))
      info <- datafile[pollutant]
      calinfo <- c(calinfo, info[!is.na(info)])
      num <- num+1
    } else if (i <= 99) {
      files[[num]] <- paste("0",i,".csv", sep = "")
      datafile <- read.csv(file.path(".", directory, files[[num]]))
      info <- datafile[pollutant]
      calinfo <- c(calinfo, info[!is.na(info)])
      num <- num+1
    } else {
      files[[num]] <- paste(i,".csv", sep = "")
      datafile <- read.csv(file.path(".", directory,files[[num]]))
      info <- datafile[pollutant]
      calinfo <- c(calinfo, info[!is.na(info)])
      num <- num+1
    }
  }
  mean(calinfo)
  
}