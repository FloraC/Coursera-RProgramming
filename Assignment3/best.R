## Finding the best hospital in a state
## Write a function called best that 
## take two arguments: the 2-character abbreviated name of a state 
## and an outcome name. 
## The function reads the outcome-of-care-measures.csv file 
## and returns a character vector with the name of the hospital 
## that has the best (i.e. lowest) 30-day mortality 
## for the specified outcome in that state. 
## The hospital name is the name provided in the Hospital.Name variable.
## The outcomes can be one of ¡°heart attack¡±, ¡°heart failure¡±, or ¡°pneumonia¡±. 
## Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings.
##  If there is a tie for the best hospital for a given outcome, 
## then the hospital names should be sorted in alphabetical order 
## and the first hospital in that set should be chosen 
## (i.e. if hospitals ¡°b¡±, ¡°c¡±,and ¡°f¡± are tied for best, 
## then hospital ¡°b¡± should be returned.)
best <- function(state, outcome) {
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
  
  ## Return hospital name in that state with lowest 30-day death
  
  ## pick up the needed info
  data_split <- split(data, data$State)
  data_state <- data_split[[state]]
  ## remove hospical that doesn't have specified info
  data_outcome <- data_state[!(data_state[,col_outcome]=="Not Available"),]
  ## change factor to numeric to compare
  ## note: have to change to character first
  data_out_numeric <- as.numeric(as.character(data_outcome[,col_outcome]))
  ## choose the minimum
  min_out <- min(data_out_numeric)
  data_result <- data_outcome[data_out_numeric==min_out, ]
  ## sort according to hn
  data_best <- as.character(sort(data_result[,col_hn]))
  ## rate
  data_best[1]
}
