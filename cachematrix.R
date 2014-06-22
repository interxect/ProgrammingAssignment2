## This package file has two functions that creates a special "matrix" object,
## computes the inverse of the matrix, and then caches the inverse to be
## reused for other purposes if necessary.


## The makeCacheMatrix function creates a special list vector with functions
## that stores the matrix and the computation of the inverse
## 
## usage:
##   y <- makeCacheMatrix
##   sets up y as the list vector
##
##   y$set(x)
##   Sets x as the matrix for inverse calculation and caching
##
##   y$get()
##   Displays the matrix data, in this case x following from above example
##
##   y$setinv(i)
##   Caches the matrix i, which should be the inverse
##
##   y$getinv()
##   Displays the cached data, which should be the inverse, i, of x
##
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function calculates the inverse of special matrix x, created
## using the makeCacheMatrix function and caches the value in the
## same matrix. If the inverse has already been cached, then it returns
## the cached value.
##
## Note: This function assumes than x has an inverse!
## 
## usage:
##   cacheSolve(y)
##     where y is the special matrix created using the makeCacheMatrix
##     function
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
