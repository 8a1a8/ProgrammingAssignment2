## This function creates a list of functions
# for cacheing matrix inversion, which can be
# a computationally intensive process

makeCacheMatrix <- function(x = matrix()) {
    
    inverted <- NULL
    
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }

    get <- function() x
    
    setinvert <- function(invert) {
        inverted <<- invert
    }
    
    getinvert <- function() inverted
    
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}

## Returns a matrix that is the inverse of 'x'
## Uses a cached inversion if available
cacheSolve <- function(x, ...) {
    invert <- x$getinvert()
    
    if(!is.null(invert)) {
        message("getting cached data")
        return(invert)
    }
    
    data <- x$get()
    invert <- solve(data, ...)
    
    x$setinvert(invert)
    invert
}
