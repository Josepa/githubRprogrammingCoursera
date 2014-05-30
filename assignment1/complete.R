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
        documents.array <- list.files(directory)
        matrix.nobs <- matrix (0, nrow = length(id), ncol = 2, dimnames = list(c(), c("id", "nobs")))
        for ( doc in documents.array ) {
                path <- paste(directory, doc, sep="/")
                current <- read.csv( path )
                if ( any( current$ID[1] == id ) ) {
                        nobs <-sum(complete.cases(current)==TRUE)
                        row = which( id == current$ID[1] )
                        matrix.nobs [row,1] <- current$ID[1]
                        matrix.nobs [row,2] <- nobs
                }
        }
        data.frame(matrix.nobs)
}
