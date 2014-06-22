## Based on the example functions provided, makeCacheMatrix and cacheSolve work together
#utilizing lexical scoping to cache the inverse of a matrix. Caching is an effective way to 
#handle potentially time-consuming computations.

## Following function creates a matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(slove) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Following function computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                m
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m        ## Return a matrix that is the inverse of 'x'
}
