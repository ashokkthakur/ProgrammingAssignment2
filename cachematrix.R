## This piece of R code is to evaluate the Inverse of a matrix and provide the outcome.
## The evaluation is done only if it's not precalculated. Otherwise the result is retrieved from the cache.

## Function "makeCacheMatrix" takes a square matrix as input and provides a list of functions and variables as output

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## Function "cacheSolve" takes the output of the above function, and provides the inverse of the matrix.
## iF the value inverse of matrix is already evaluated, it fetches the result from cache.

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
     ## Return a matrix that is the inverse of 'x'
}
