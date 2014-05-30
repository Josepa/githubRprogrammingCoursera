## Cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse, 
## which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        solve <- NULL
        set <- function(y){
                x <<- y
                n <<-NULL
        }
        get <- function() {x}
        setsolve <- function(y) { solve <<- y }
        getsolve <- function() solve
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setsolve(m)
                m ## Return a matrix that is the inverse of 'x'
}