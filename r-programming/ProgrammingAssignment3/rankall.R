rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # Keep columns only from Hospital.Name, State, Heart.Attack, Heart.Failure and Pneumonia
  data <- data[, c(2, 7, 11, 17, 23)]
  
  ## Check that state and outcome are valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% valid_outcomes) {
    stop ("invalid outcome")
  }
  if (class(num) == "character") {
    if (!(num == "best" || num == "worst")) {
      stop ("invalid rank")
    }
  }
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the (abbreviated) state name
  # Remove columns by outcome, only left HospitalName and Deaths by outcome
  if (outcome == "heart attack") {
    data = data[, c(1, 2, 3)]
  } else if (outcome == "heart failure") {
    data = data[, c(1, 2, 4)]
  } else if (outcome == "pneumonia") {
    data = data[, c(1, 2, 5)]
  }
  names(data)[3] = "DeathRate"
  data[, 3] = suppressWarnings(as.numeric(data[, 3]))
  
  # Remove NA rows
  data = data[!is.na(data$DeathRate),]
  
  split_data = split(data, data$State)
  new = lapply(split_data, function(x, num) {
    # Order by DeathRate and HospitalName
    x = x[order(x$DeathRate, x$Hospital.Name),]
    
    # Return specified Hospital.Name
    if (class(num) == "character") {
      if (num == "best") {
        return (x$Hospital.Name[1])
      }
      else if (num == "worst") {
        return (x$Hospital.Name[nrow(x)])
      }
    }
    else {
      return (x$Hospital.Name[num])
    }
  }, num)
  
  # Return data frame
  return (data.frame(hospital = unlist(new), state = names(new)))
}