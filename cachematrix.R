## Provide a matrix that can cache its inverse

## Make a caching matrix

# A caching matrix is a matrix that can cache its inverse (the value returned
# by the solve() function). The returned datastructure is a list of functions,
# that is:
#
# - get and set: get and set the matrix values
# - getinv and setinv: get and set the matrix inverse
# 
# This is to be used using cacheSolve().
#
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() { x }
  setinv <- function(inv_) { inv <<- inv_ }
  getinv <- function() { inv }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## A caching variant of the solve() function.
#
# Computes the inverse of the given caching matrix x, which is a matrix
# made by makeCacheMatrix(). The function returns the inverse of
# the matrix, if possible from a cached value calculated in an earlier
# invocation.
#
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    return(inv)
  }
}