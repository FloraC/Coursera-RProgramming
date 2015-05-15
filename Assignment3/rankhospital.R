## Write a function called rankhospital that takes three arguments:
## the 2-character abbreviated name of a state (state), 
## an outcome (outcome), and the ranking of a hospital in that state 
## for that outcome (num).
## The function reads the outcome-of-care-measures.csv file and 
## returns a character vector with the name
## of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  case_outcome <- c("heart attack","heart failure","pneumonia")
  if (sum(state == data[["State"]])==0) {
    stop("invalid state")
  }
  if (sum(outcome == case_outcome)==0) {
    stop("invalid outcome")
  }
  
  ## col for specified outcome
  if (outcome == "heart attack") {
    col_outcome <- 11
  } else if (outcome == "heart failure") {
    col_outcome <- 17
  } else {
    col_outcome <- 23
  }
  ## col for hospital name
  col_hn <- 2
  
  ## Return hospital name in that state with the given rank
  ## pick up the needed info
  data_split <- split(data, data$State)
  data_state <- data_split[[state]]
  ## remove hospical that doesn't have specified info
  data_outcome <- data_state[!(data_state[,col_outcome]=="Not Available"),]
  ## change factor to numeric to compare
  ## note: have to change to character first
  data_outcome[,col_outcome] <- as.numeric(as.character(data_outcome[,col_outcome]))
  data_outcome <- data_outcome[,c(col_hn,col_outcome)]
  ## Sort the result increasing
  ind_inc <- sort(data_outcome[,2], index.return = TRUE)$ix
  ## choose the corresponding num
  if (num == "best") {
    data_rate <- data_outcome[ind_inc[1],2]
    ## sort according to hn
    data_result <- sort(data_outcome[data_outcome[,2]==data_rate,1])
    ## 30-day death rate
    as.character(data_result[1])
  } else if (num == "worst") {
    data_rate <- data_outcome[ind_inc[length(ind_inc)],2]
    ## sort according to hn
    data_result <- sort(data_outcome[data_outcome[,2]==data_rate,1])
    ## 30-day death rate
    as.character(data_result[length(data_result)])
    
  } else if (num > nrow(data_outcome)) {
    data_rate <- NA
    ## 30-day death rate
    data_rate
  } else {
    data_rate <- data_outcome[ind_inc[num],2]
    count <- sum(data_outcome[,2]<data_rate)
    ## sort according to hn
    data_result <- sort(data_outcome[data_outcome[,2]==data_rate,1])
    ## 30-day death rate
    as.character(data_result[num-count])
  }

}
