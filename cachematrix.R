## The two functions below will cache a inverse of a matrix
## The first one creates a special matrix that can cache its inverse
## the second one checks if the inverse was calculated and save some
## data computing

# for inverse calculation, we need the matlib package
install.packages("matlib")
library(matlib)

## Lets create a matrix

makeCacheMatrix <- function(x = matrix()) {
    datasetinverse <- NULL                   
    set <- function(y) {
            x<<-y
            datasetinverse <<- NULL
    }
    get <- function() {x}
    setinverse <- function(solve) {datasetinverse <<- solve(x)}
    getinverse <- function() {datasetinverse}
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## This function checks if the inverse was already calculated
## in this way we can save some time in our data processing

cacheSolve <- function(x, ...) {
    datasetinverse <- x$getinverse()
    if(!is.null(datasetinverse)){
        message("getting cached data")
        return(datasetinverse)
    }
  
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(datasetinverse)
    inv
}
        ## Return a matrix that is the inverse of 'x'
