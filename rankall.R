library(dplyr)
library(readr)
rankall <- function(outcome, num = "best") {
  ## Read data
  data <- read.csv("~/R/ProgrammingAssignment3/outcome-of-care-measures.csv", colClasses="character")
  
  ## Convert data type
  data[, 11] <- as.numeric(data[, 11]) # Heart Attack
  data[, 17] <- as.numeric(data[, 17]) # Heart Failure
  data[, 23] <- as.numeric(data[, 23]) # Pnumonia
  
  ## Set empty data frame
  result <- data.frame()
  
  state_list <- sort(unique(data$State)) # A list of states
  state_len <- length(state_list) # How many states in the list
  
  ## Set valid outcome input
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check if outcome is valid
  if (! outcome %in% valid_outcome) {
    stop("Invalid Outcome!")
  }
  
  ##Creating data frame based on outcome type
  else{
    if (outcome == "heart attack") {
    df <- data[, c(2, 7, 11)]
  }
    else if (outcome == "heart faliure") {
    df <- data[, c(2, 7, 17)]
  }
    else if (outcome == "pneumonia"){
    df <- data[, c(2, 7, 23)]
    }
  }
  
  ## For each state, find the hospital of the given rank
  for (i in 1: state_len) {
    df <- filter(df, df$State == state_list[i])
    
  }
}