## capability to cache expensive function resutls
## so that multiple invocations don't compute the results again

## create a function that is capable of caching matrix inversion results
## and return them again when invoked without modification to the data
## the function returns a list with 4 methods to set data, get data, compute
## inverse and return inverse. It uses lexical scoping to cache data and
## leverages <<- operator to update the cache in parent scope.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) inverse <- inv
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## return the cached inverse result and if nothing is cached
## compute the inverse, cache it and return the results.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data)
    x$set(inverse)
    inverse
}
