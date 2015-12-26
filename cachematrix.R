## This set of functions caches the inverse of a matrix from stored data if available, 
## or it calculates it from scratch if the data was not cached.  

## This function creates a matrix that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the "matrix" given by the makeCacheMatrix function.
## If the inverse was previously calculated from an unchanged matrix
## then the cachesolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data... please hold")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
