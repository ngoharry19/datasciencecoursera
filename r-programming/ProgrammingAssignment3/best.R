best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # Keep columns only from Hospital.Name, State, Heart.Attack, Heart.Failure and Pneumonia
  data <- data[, c(2, 7, 11, 17, 23)]
  ## Check that state and outcome are valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!state %in% data$State) {
    stop("invalid state")
  } else if (!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
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
  
  # Rename outcome row for simplicity and change to numeric
  names(data)[2] = "DeathRate"
  data[, 2] = suppressWarnings(as.numeric(data[, 2]))
  # Remove NA rows
  data = data[!is.na(data$DeathRate),]
  # Order by DeathRate and return first Hospital.Name
  data = data[order(data$DeathRate, data$Hospital.Name), ]
  return(data$Hospital.Name[1])
}
