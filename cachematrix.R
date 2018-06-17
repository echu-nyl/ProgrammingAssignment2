## The following pair of functions cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
 
## Base structure of code is based on "makeVector" function given in assignment, with
## the following modifications:
##   1. Function takes matrix() object instead of vector() object
##   2. Replace references to "mean" with "inverse"
##   3. Change corresponding variable "m" to "i"
 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix)
## has not changed, then the cacheSolve should retrieve the inverse from the cache.
 
## Base structure of code is based on "cachemean" function given in assignment, with
## the following modifications:
##   1. Replace functionality and references to "mean" with "solve" and "inverse"
##   2. Change corresponding variable "m" to "i"
 
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
