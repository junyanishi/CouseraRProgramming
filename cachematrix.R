## functions to cache the inverse of a matrix
## Firstly use makeCacheMatrix to cache the matrix,
## Then use cachesolve to calculate the inverse of the matrix returned by makecacheMatrix.

## function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## function to compute the inverse of the special matrix
## if the inverse has already been calculated, then return the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
