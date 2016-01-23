## These functions create cache-enabled versions of a function to compute the inverse of a
## square, invertible matrix and the corresponding input to that function.  With these
## functions, the Solve function will only need to be applied once for a given matrix.

## makeCacheMatrix takes a matrix, assumed to be square and invertible, and converts it to a list
## form that contains information about whether the inverse of the input matrix has been previously
## computed by a cache-award function leveraging this format.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns the inverse of a square, invertible matrix that has been appropriately
## formatted by the makeCacheMatrix function.  If cacheSolve is operating on the input for the 
## first time, it also stores the result in the input object (it's "cache").  If cacheSolve
## has previously been applied, cacheSolve returns the previously calculated result from cache,
## rather than evaluating the Solve function again.

cacheSolve <- function(x, ...) {
    s <- x$getinv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}
