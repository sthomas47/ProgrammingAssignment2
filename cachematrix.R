## Put comments here that give an overall description of what your
## functions do
#
# Create a special matrix object that can cache the inverse of the matrix 'x'
# Example of using this function together with cacheSolve
#   source('cachematrix.R')
#   m1<-matrix(1:4,2,2)
#   mobj<-makeCacheMatrix(m1)
#   cacheSolve(mobj)
#   cacheSolve(mobj)
# cacheSolve computes and returns the inverse of m1, and caches it.
# Subsequent calls return the cached inverse.
#   

## Write a short comment describing this function
# Create a special matrix object that can cache the inverse of the matrix 'x'
makeCacheMatrix <- function(x = matrix()) {
    ## make a special matrix object that can cache the matrix inverse
    m <- NULL  # m is the inverse of the matrix, x is the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# Compute, cache and return inverse of matrix for special matrix object.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x', where 'x' is a special matrix object
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
