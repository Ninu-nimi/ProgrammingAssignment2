## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a matrix object and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv1) inv <<- inv1
  getinv <- function() inv 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## checkes cache for the avialibility of inverse of the 
## passed matrix if not found calculates 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv))
    {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
