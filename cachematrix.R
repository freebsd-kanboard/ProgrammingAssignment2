## The following functions allow to create an inverse of a matrix and to cache
## the results. It might be useful in case of big matrices: allows to avoid
## the re-processing of the matrix through caching the result.

## !!!
## an assumption is made that the provided matrix is always invertible
## !!!


## makeCacheMatrix is an 'access' function allowing to manipulate the matrix
## .set() allows to set a value, just like calling makeCacheMatrix(m)
## .get() returns the current matrix
## .setinverse() sets the inverse of the matrix
## .getinverse() returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    # setter of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # getter of the matrix
    get <- function() x

    # setter of the inverse
    setinverse <- function(inverse) i <<- inverse

    # getter of the inverse
    getinverse <- function() i

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is returning inverse of 'x' matrix.
## inverse of the matrix is being cached to avoid reprocessing and save time
## ... parameters are being passed to solve() function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## 'x' is assumed to be always invertible

    # check if already in cache...
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }

    # ... and if not in cache then get the inverse, cache and return the result
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
