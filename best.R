library(dplyr)
library(readr)
best <- function(state, outcome){
  # Read data
  data <- read.csv("~/R/ProgrammingAssignment3/outcome-of-care-measures.csv", colClasses="character")
  
  #Pick up heart attack, heart failure, and pneumonia data
  data[, 11] <- as.numeric(data[, 11]) # Heart Attack
  data[, 17] <- as.numeric(data[, 17]) # Heart Failure
  data[, 23] <- as.numeric(data[, 23]) # Pnumonia
  
  # Set valid outcome input
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  
  # Check the state and outcome are valid
  if (!state %in% data$State) {
    stop("Invalid State!")
  }
  else if (!outcome %in% valid_outcome) {
    stop("Invalid Output!")
  }
  # Return hospital name in that state with lowest 30-day death
  else {
    if (outcome == "heart attack") {
      temp <- filter(data, data$State == state)
      i <- which.min(temp[, 11])
      hospital_name <- temp[i, 2]
    }
    else if (outcome == "heart failure") {
      temp <- filter(data, data$State == state)
      i <- which.min(temp[, 17])
      hospital_name <- temp[i, 2]
    }
    else if (outcome == "pneumonia") {
      temp <- filter(data, data$State == state) # Filter the data frame by state
      i <- which.min(temp[, 23]) #index of min. row
      hospital_name <- temp[i, 2] # Get hospital name
    }
  }
  hospital_name
}