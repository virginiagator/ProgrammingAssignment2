## Put comments here that give an overall description of what your
## functions do

## This function sources an object that can cache it's own inverse

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(z) {
        x <<- z
        Inv <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) Inv <<- inverse
        getInverse <<- function() Inv
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## The function calculates the inverse of the matrix x

cacheSolve <- function(x, ...) {
        Inv <- x$getInverse()
        if (!is.null(Inv)) {
        message("get cached data")
        return(Inv)
        }
        
        mat <- x$get()
        Inv <- solve(mat, ...)
        x$setInverse(Inv)
        Inv
}

