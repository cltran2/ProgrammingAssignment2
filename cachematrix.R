## Provides functions that support caching of matrix inverse

## Creates a wrapper for the specified matrix with helper methods to support caching of its inverse

makeCacheMatrix <- function(x = matrix()) {
    cachedInv <- NULL

    set <- function (value) {
        x <<- value
        cachedInv <<- NULL
    }

    get <- function () x

    setInverse <- function (inv) {
        cachedInv <<- inv
    }

    getInverse <- function () cachedInv

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns a matrix that is the inverse of 'x' if it exists in the cache;
## otherwise calculates the inverse matrix and puts the result into the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        result <- x$getInverse()
        if (!is.null(result)) {
            message('<- cached inverse')
            return(result)
        }
        data <- x$get()
        result <- solve(data, ...)
        x$setInverse(result)
        result
}
