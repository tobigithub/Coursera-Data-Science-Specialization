# Write a function that reads a directory full of files and reports the number of completely 
# observed cases in each data file. The function should return a data frame where the first 
# column is the name of the file and the second column is the number of complete cases. 
# A prototype of this function follows

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  complete.table <- data.frame()
  for (i in seq_along(id)){
    path <- paste0(directory, "/", sprintf("%03d", id[i]), ".csv")
    tempdata <- read.csv(path)
    tempdata <- na.omit(tempdata)
    tempdata <- c(id[i], nrow(tempdata))
    complete.table <- rbind(complete.table, tempdata)
  }
  colnames(complete.table) <- c("id", "nobs")
  return(complete.table)
}


