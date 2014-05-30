corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        documents.array <- list.files(directory)
        cv <- vector (mode="numeric", length=0)
        for ( doc in documents.array ) {
                path <- paste(directory, doc, sep="/")
                current <- read.csv( path )
                current <- current [complete.cases(current),]
                print (nrow(current))
                if ( nrow (current) > threshold) { print (cv)
                        cv <- c(cv, cor(current$sulfate, current$nitrate))
                }
        }
       cv
}