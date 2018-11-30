## The following functions should be used to cache the inverse of a matrix 
## and return it if a) is a square matrix and b) is invertible 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solveMatrix) inv <<-solveMatrix
        getinv <- function() inv
        list	(set = set,
              get = get,
              setinv = setinv,
              getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and 
## the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached Matrix")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)
        inv
}
        ## Return a matrix that is the inverse of 'x'

> m <- matrix(rnorm(4),2,2)

> m
[,1]       [,2]
[1,]  0.9080868  1.9947391
[2,] -0.7572151 -0.3874604

> m1 <- makeCacheMatrix(m)
> cacheSolve(m1)
[,1]       [,2]
[1,] -0.3344215 -1.7216822
[2,]  0.6535611  0.7837802
> cacheSolve(m1)
Getting cached Matrix
[,1]       [,2]
[1,] -0.3344215 -1.7216822
[2,]  0.6535611  0.7837802
> 
