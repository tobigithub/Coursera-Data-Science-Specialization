# Write a function that takes a directory of data files and a threshold for complete cases and
# calculates the correlation between sulfate and nitrate for monitor locations where the number
# of completely observed cases (on all variables) is greater than the threshold. The function
# should return a vector of correlations for the monitors that meet the threshold requirement.
# If no monitors meet the threshold requirement, then the function should return a numeric vector
# of length 0. A prototype of this function follows

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  filestoread <- list.files(directory)
  cor.vec <- vector()
  for (i in seq_along(filestoread)){
    path <- paste0(directory, "/", filestoread[i])
    tempdata <- read.csv(path)
    # Test threshold
    if (length(which(complete.cases(tempdata)==TRUE)) > threshold){
      cor.vec[i] <- cor(x=tempdata["sulfate"], y=tempdata["nitrate"], 
			use="complete.obs")
    }
  }
  return(na.omit(cor.vec))
}

