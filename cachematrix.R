## These functions are responsible for taking a matrix, calculating
## its inverse, and caching the result, so that subsequent calls for
## inversion of the same matrix will return the cached inverse, thereby
## saving the expense of recalculating it.
## Stores a matrix and creates function for setting and retrieving
## its inverse to/from a parent-scoped cache
## Args:
##   x: An invertible  matrix
## Returns:
##   Functions for setting & getting a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Declare the inverse and set to NULL for now
  i <- NULL
  # Sets x to be the supplied matrix in closure scope, and set the inverse to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # Returns the matrix in the closure's scope
  get <- function() x
  # Sets the cached inverse in closure scope
  setInverse <- function(inverse) i <<- inverse
  # Retrieves the cached inverse from parent scope (or NULL if not yet set)
  getInverse <- function() i
  # Return setters and getters for the matrix and its inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Takes an invertible matrix and tries to retrieve its inverse from the cache.
## If it is not cached, then the inverse is calculated locally and added to 
## the cache
## Args:
##   x: An invertible matrix to be inverted
## Returns:
##   The inverse of the supplied matrix

cacheSolve <- function(x, ...) {
  # Attempt to retrieve inverse from the cache
  i <- x$getInverse()
  if (!is.null(i)) {
    # Return the cached inverse
    return(i)
  }
  # Inverse is not cached, invert it now and add that to the cache
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  # Return the newly-calculated inverse
  i
}
