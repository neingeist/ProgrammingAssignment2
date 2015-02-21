## Provide a matrix that can cache its inverse

## Make a caching matrix
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

## A caching vairant of the solve() function (computing the inverse)
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