## 1 Plot the 30-day mortality rates for heart attack
## Read the outcome data into R via the read.csv function and look at the first few rows
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
n_col <- ncol(outcome)
name_col <- names(outcome)
## to make a simple histogram of the 30-day death rates 
## from heart attack
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])
