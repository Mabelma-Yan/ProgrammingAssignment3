rankhospital <- function(state, outcome, num = "best"){
  ## Read data
  data <- read.csv("~/R/ProgrammingAssignment3/outcome-of-care-measures.csv", colClasses="character")
  
  ## Convert data type
  data[, 11] <- as.numeric(data[, 11]) # Heart Attack
  data[, 17] <- as.numeric(data[, 17]) # Heart Failure
  data[, 23] <- as.numeric(data[, 23]) # Pnumonia
  
  ## Set empty data frame
  result <- data.frame()
  
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
  
  temp <- filter(df, df$State == state)
  temp <-arrange(temp, temp[, 3])
  
  if (num == "best"){
    i <- which.min(temp[, 3])
    result <- temp[i, ]
  }
  else {
    if (num == "worst"){
      i <- which.max(temp[, 3])
      result <- temp[i, ]
    }
    else if (num != "best" | num != "worst"){
      result <-temp[num, ]
    }
  }
  result
}