## This function is used for Caching the inverse of a Matrix,
## which is a time consuming process if computed repeatedly.  

## This finctoon creates a special "matrix" object that can be used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
              inv <- NULL
              
              set <- function(y) {
                x <<- y
                inv <<- NULL
              }
              get <- function() x
              setInverse <- function(inverse) inv <<- inverse 
              getInverse <- function() inv 
              list ( set = set,
                     get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
              
}


## The cacheSolve function below computes the inverse of the special "matrix" from above
## and if the inverse is already compluted it will retrive the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv <- x$getInverse()
       if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
       }
       data <- x$get()
       inv  <- solve(data, ...)
       x$setInverse(inv)
       inv
  }
