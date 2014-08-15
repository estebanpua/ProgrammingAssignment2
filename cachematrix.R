## Programming Assignment 2:
##   See example below

## Stores a matrix (it is assumed that the matrix supplied is always invertible)
## and its inverse
## Provides the following functions:
## - set(x): assign a matrix x to the object (and set its inverse to NULL)
## - get(): return the stored matrix
## - setinverse(inv): store the matrix 'inv' as the inverse matrix
## - getinverse(): return the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) ix <<- inv
        getinverse <- function() ix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function that needs an object built with the makeCacheMatrix (with an
## inversible matrix) and returns its inverse. If the inverse was calculated
## previously, a cached value will be returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ix <- x$getinverse()
        if(!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }
        data <- x$get()
        ix <- solve(data, ...)
        x$setinverse(ix)
        ix
}

## Example:
## > x <- matrix(c(1, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3, ncol = 3)
## mcm <- makeCacheMatrix(x)
## > mcm$get()
## [,1] [,2] [,3]
## [1,]    1    1    0
## [2,]    1    0    1
## [3,]    0    1    0
## > cacheSolve(mcm)
## [,1] [,2] [,3]
## [1,]    1    0   -1
## [2,]    0    0    1
## [3,]   -1    1    1
## > cacheSolve(mcm)
## getting cached data
## [,1] [,2] [,3]
## [1,]    1    0   -1
## [2,]    0    0    1
## [3,]   -1    1    1
