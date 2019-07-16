## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getInverse()
        mx <- x$getMatrix()
        if(!is.null(mx) & !is.null(mi)) {
                message("Matrix data has not changed, so getting cached inverse matrix")
                return(mi)
        } else {
                data <- x$getMatrix()
                mi <- solve(data)
                x$setInverse(mi)
                mi
        }
}
