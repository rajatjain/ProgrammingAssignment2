# This function provides a list of functions to perform cache based matrix operations:
#  - get: Return the matrix from the cache (if any).
#  - set: Set a new value of matrix in the cache. Updates the inverse to NULL.
#  - getinverse: Return the inverse of the matrix from the cache.
#  - setinverse: Set the new value of inverse in the cache.
makeCacheMatrix <- function(x = matrix()) {
  inverse <<- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(newinverse) inverse <<- newinverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Returns the inverse of the matrix. It may either calculate the inverse of the
# cached matrix if no cache is not present, put into the cache and return,
# or return the inverse directly if it is present in the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
