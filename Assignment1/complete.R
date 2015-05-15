## Part2 of Assignment 1
## Write a function that reads a directory full of files 
## and reports the number of completely observed cases in each data file. 
## The function should return a data frame where the first column 
## is the name of the file and the second column is the number 
## of complete cases. A prototype of this function follows

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  ## load the needed files
  files <- 1:length(id)
  
  calinfo <- NULL
  dep_1 <- "sulfate"
  dep_2 <- "nitrate"
  num <- 1:1
  Result <- data.frame(id, 1:length(id))
  names(Result) <- c("id", "nobs")

  ## define the file names and read
  for (i in id) {
    if (i <= 9) {
      files[[num]] <- paste("00",i,".csv", sep = "")
      datafile <- read.csv(file.path(".", directory,files[[num]]))
      info1 <- datafile[dep_1] ## sulfate data
      info2 <- datafile[dep_2] ## nitrate data      
      calinfo <- (!is.na(info1)) & (!is.na(info2))
      
      Result[["nobs"]][num]<- sum(calinfo)
  
    } else if (i <= 99) {
      files[[num]] <- paste("0",i,".csv", sep = "")
      datafile <- read.csv(file.path(".", directory, files[[num]]))
      info1 <- datafile[dep_1] ## sulfate data
      info2 <- datafile[dep_2] ## nitrate data      
      calinfo <- (!is.na(info1)) & (!is.na(info2))
      Result[["nobs"]][num]<- sum(calinfo)
      
    } else {
      files[[num]] <- paste(i,".csv", sep = "")
      datafile <- read.csv(file.path(".", directory,files[[num]]))
      info1 <- datafile[dep_1] ## sulfate data
      info2 <- datafile[dep_2] ## nitrate data      
      calinfo <- (!is.na(info1)) & (!is.na(info2))
      
      Result[["nobs"]][num] <- sum(calinfo)
      
    }
  num <- num+1
  }
  Result
  
}