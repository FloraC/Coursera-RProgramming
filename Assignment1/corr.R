## Part3 of Assignment1
## Write a function that takes a directory of data files 
## and a threshold for complete cases and calculates the 
## correlation between sulfate and nitrate for monitor locations 
## where the number of completely observed cases (on all variables) 
## is greater than the threshold. The function should return a vector 
## of correlations for the monitors that meet the threshold requirement. 
## If no monitors meet the threshold requirement, 
## then the function should return a numeric vector of length 0. 
## A prototype of this function follows

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  ## pick up those above threshold
  data <- complete(directory)
  data_id <- data[["id"]]
  data_nobs <- data[["nobs"]]
  data_id <- data_id[data_nobs > threshold]
  
  
  ## load the needed files
  files <- 1:length(data_id)
  
  calinfo <- NULL
  dep_1 <- "sulfate"
  dep_2 <- "nitrate"
  num <- 1:1
  Result <- vector("numeric", length = 0)
  
  ## define the file names and read
  for (i in data_id) {
    if (i <= 9) {
      files[[num]] <- paste("00",i,".csv", sep = "")
      datafile <- read.csv(file.path(".", directory,files[[num]]))
      info1 <- datafile[dep_1] ## sulfate data
      info2 <- datafile[dep_2] ## nitrate data
      info_na <- (!is.na(info1)) & (!is.na(info2))
      calinfo <- cor(info1[info_na], info2[info_na])
      
      Result <- c(Result, calinfo)
      
    } else if (i <= 99) {
      files[[num]] <- paste("0",i,".csv", sep = "")
      datafile <- read.csv(file.path(".", directory, files[[num]]))
      info1 <- datafile[dep_1] ## sulfate data
      info2 <- datafile[dep_2] ## nitrate data      
      info_na <- (!is.na(info1)) & (!is.na(info2))
      calinfo <- cor(info1[info_na], info2[info_na])
      Result <- c(Result, calinfo)
      
    } else {
      files[[num]] <- paste(i,".csv", sep = "")
      datafile <- read.csv(file.path(".", directory,files[[num]]))
      info1 <- datafile[dep_1] ## sulfate data
      info2 <- datafile[dep_2] ## nitrate data      
      info_na <- (!is.na(info1)) & (!is.na(info2))
      calinfo <- cor(info1[info_na], info2[info_na])
      
      Result <- c(Result, calinfo)
      
    }
    num <- num+1
  }
  Result
}