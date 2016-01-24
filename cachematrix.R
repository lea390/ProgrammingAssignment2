## This set of functions computes the inverse of a matrix as long as the matrix
## is squared and it is possible to find the solution.
## The result of a inversed matrix is cached so that it does not have to be
## computed again if needed later on. It thereby reduces the time of the 
## operation to inverse the same matrix again.

## This function creates a matrix object that can cache the inverse once it is
## computed.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() {
        x
    }
    setsolve <- function(solve) {
        s <<- solve
    }
    getsolve <- function() {
        s
    }
    list(set = set, 
         get = get, 
         setsolve = setsolve, 
         getsolve = getsolve)
}


## This function takes a matrix created by the makeCacheMatrix function.
## The function checks if a cache of the inverse matrix exists. If a cache 
## exists, it returns the cache. If no cache exists, it computes the inverse
## and caches it.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
