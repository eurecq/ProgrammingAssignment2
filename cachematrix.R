## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Create an object which caches matrix inverse of `x`
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversed) inv <<- inversed
  getinv <- function() inv
  ## returns a list with functions as elements
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Return matrix inverse result with an cached object as input
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    ## Cached result has been found
    message("getting cached data")
    return(inv)
  }
  
  ## No cached matrix inverse, then calculate it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv      
}
