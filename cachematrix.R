## MakeCacheMatrix is a utility function that caches a single matrix.
## Once MakeCacheMatrix is called, we create setters and getter functions
## which are returned as a list. These can be called by any other part of the
## program to set new value, or retrieve existing value. 
## The logic to test whether a result value (in this case matrix inverse)
## exists or one has to be computed and stored, is provided by the caller.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {  ## store matrix to invert
        x <<- y
        m <<- NULL
    }
    ## return matrix to invert
    get <- function() x 
    ## Set inverted matrix
    setInv <- function(inv) m <<- inv  
    ## get inverted matrix
    getInv <- function() m  
    ## expose setter and getter functions to caller
    list(set = set, get = get,  
         setInv = setInv,
         getInv = getInv)
}


## Get the inverse of a previously specified square matrix, or compute and cache the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
