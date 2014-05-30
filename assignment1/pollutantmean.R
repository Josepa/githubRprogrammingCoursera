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
  
  documents.array <- list.files(directory)
  data.list <- list()
  for ( doc in documents.array ) {
          path <- paste(directory, doc, sep="/")
          current <- read.csv( path )
          if ( any( current$ID[1] == id ) ) {
                  data.list[[length(data.list) + 1L]] <- current
          }
  }
  result <- 0
  if (length(data.list) > 0) {
    data.matrix <- do.call(rbind, data.list)
    result <- mean(data.matrix[,pollutant], na.rm=TRUE)
  }
  result
}