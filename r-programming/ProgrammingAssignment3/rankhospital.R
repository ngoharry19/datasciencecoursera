rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # Keep columns only from Hospital.Name, State, Heart.Attack, Heart.Failure and Pneumonia
  data <- data[, c(2, 7, 11, 17, 23)]
  
  ## Check that state and outcome are valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!state %in% data$State) {
    stop ("invalid state")
  } else if (!outcome %in% valid_outcomes) {
    stop ("invalid outcome")
  }
  if (class(num) == "character") {
    if (!(num == "best" || num == "worst")) {
      stop ("invalid rank")
    }
  }
  ## Return hospital name in that state with the given rank 30-day death rate
  # Keep rows only in the specified state
  data = data[data$State == state,]
  
  # Conditional if else statements for each outcome and keep columns Hospital.Name and Outcome
  if (outcome == "heart attack") {
    data = data[, c(1, 3)]
  } else if (outcome == "heart failure") {
    data = data[, c(1, 4)]
  } else if (outcome == "pneumonia") {
    data = data[, c(1, 5)]
  }
  names(data)[2] = "DeathRate"
  data[, 2] = suppressWarnings(as.numeric(data[, 2]))
  
  # Remove NA rows
  data = data[!is.na(data$DeathRate),]
  
  # for num ranks, if num > nrows then return NA
  if (class(num) == "numeric" && num > nrow(data)) {
    return (NA)
  }
  
  # Order by DeathRate and Hospital.Name
  data = data[order(data$DeathRate, data$Hospital.Name),]
  
  # Return Hospital.Name
  if (class(num) == "character") {
    if (num == "best") {
      return (data$Hospital.Name[1])
    }
    else if (num == "worst") {
      return (data$Hospital.Name[nrow(data)])
    }
  }
  else {
    return (data$Hospital.Name[num])
  }
}