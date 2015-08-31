pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  readings <- data.frame()
  for (i in seq_along(id)){
    path <- paste0(directory, "/", sprintf("%03d", id[i]), ".csv")
    tempdata <- read.csv(path)
    readings <- rbind(readings, tempdata)
  }
  # Keep only requested pollutant
  readings <- readings[pollutant]
  return(mean(readings[,1], na.rm=T))
}