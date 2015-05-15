## The function reads the outcome-of-care-measures.csv file 
## and returns a 2-column data frame containing the hospital 
## in each state that has the ranking specified in num. 
## For example the function call rankall("heart attack", "best") 
## would return a data frame containing the names of the hospitals that
## are the best in their respective states for 30-day heart attack death rates. 
## The function should return a value for every state (some may be NA). 
## The first column in the data frame is named hospital, which contains
## the hospital name, and the second column is named state, which contains the 2-character abbreviation for
## the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
## hospitals when deciding the rankings.
## The rankall function should handle ties in the 30-day mortality rates 
## in the same way that the rankhospital function handles ties.
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
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
  ## col for state name
  col_state <- 7
  ## For each state, find the hospital of the given rank
  
  ## pick up the needed info
  data_split <- data[,c(col_hn, col_state, col_outcome)]
  ## remove hospical that doesn't have specified info
  data_outcome <- data_split[!(data_split[,3]=="Not Available"),]
  data_outcome[,3] <- as.numeric(as.character(data_outcome[,3]))
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  ## define a data frame
  data_result <- data.frame(hospital="hospital",state="state")
  result_hospital <- NULL
  result_state <- NULL
  ## split data in states
  data_state <- split(data_outcome,data_outcome$State)
  state_names <- names(data_state)
  
  for (sname in state_names) {
    ## Sort the result increasing
    ind_inc <- sort(data_state[[sname]][,3], index.return = TRUE)$ix
    ## choose the corresponding num
    if (num == "best") {
      data_rate <- data_state[[sname]][ind_inc[1],3]
      ## sort according to hn
      data_hp <- sort(data_state[[sname]][data_state[,3]==data_rate,1])
      ## 30-day death rate
      data_hn <- as.character(data_hp[1])
      data_result<- c(data_result, data.frame(hospital=data_hn,state=sname))
      result_hospital <- c(result_hospital,data_hn)
      result_state <- c(result_state, as.character(sname))
    } else if (num == "worst") {
      data_rate <- data_state[[sname]][ind_inc[length(ind_inc)],3]
      ## sort according to hn
      data_hp <- sort(data_state[[sname]][data_state[[sname]][,3]==data_rate,1])
      ## 30-day death rate
      data_hn <- as.character(data_hp[1])
      data_result<- c(data_result, data.frame(hospital=data_hn,state=sname)) 
      result_hospital <- c(result_hospital,data_hn)
      result_state <- c(result_state, as.character(sname))
    } else if (num > nrow(data_state[[sname]])) {
      data_result<- c(data_result, data.frame(hospital=NA,state=sname))
      result_hospital <- c(result_hospital,NA)
      result_state <- c(result_state, as.character(sname))
    } else {
      data_rate <- data_state[[sname]][ind_inc[num],3]
      count <- sum(data_state[[sname]][,3]<data_rate)
      ## sort according to hn
      data_hp<- sort(data_state[[sname]][data_state[[sname]][,3]==data_rate,1])
      ## 30-day death rate
      data_hn <- as.character(data_hp[num-count])
      data_result<- c(data_result, data.frame(hospital=data_hn,state=sname))
      result_hospital <- c(result_hospital,data_hn)
      result_state <- c(result_state, as.character(sname))
    }
  }
  data_result[["hospital"]] <- data_result[["hospital"]][-1]
  data_result[["state"]] <- data_result[["statel"]][-1]
  result <- data.frame(hospital = result_hospital, state= result_state)
  result
}