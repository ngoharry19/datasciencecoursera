corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the 
  ## number of completely observed observations (on all
  ## variables) requi?red to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  corr_vec <- numeric(0)
  file_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  complete_cases <- complete(directory)
  complete_cases <- subset(complete_cases, complete_cases$nobs > threshold)
  
  for(i in complete_cases$id) {
    data <- read.csv(file_list[i])
    data <- subset(data,complete.cases(data))        
    sulfate_data <- data["sulfate"]
    nitrate_data <- data["nitrate"]
    corr_vec <- c(corr_vec, cor(sulfate_data, nitrate_data))
  }
  corr_vec
}