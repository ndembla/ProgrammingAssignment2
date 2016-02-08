## This file computes inverse of a matrix and caches the result. The result
## is cached for fast retrieval.

## makeCacheMatrix function defines list of functions to get and set matrix and
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setMatrix <- function(y) {
          x <<- y 
          i <<- NULL}
    getMatrix <- function() {
          x }
    setInverse <- function(z) {
         i <<- z }
    getInverse <- function() {
         i }

    list(setMatrix = setMatrix(), getMatrix = getMatrix(), 
         setInverse = setInverse(), getInverse = getInverse())
}


## cacheSolve computes the inverse of a matrix. It uses a cache result 
## whereever possible 

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (!isNan(inverse)) {
            return inverse
            }
        matrixval <- x$getMatrix()
        x$setMatrix(matrixval)
        inverse <- solve(matrixval)
        x$setInverse(inverse)

        ## Return a matrix that is the inverse of 'x'
        inverse
}
