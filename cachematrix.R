## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  cache <- NULL
  
  # store a new matrix
  setMatrix <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  # return the stored matrix
  getMatrix <- function () { x }
  
  # cache the argument
  setInverse <- function(problem) {
    cache <<- problem
  }
  
  # get the catched value
  getInverse <- function () { cache }
  
  # return a list where each element of the list is one of the defined functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # get the cached value
  cache <- x$getInverse()
  
  # if we have the cached value, we return it and finish
  if (!is.null(cache)) {
    message("Getting cached data")
    return (cache)
  }
  
  # if not, we must calculate the value of the inverse and store it in the inverse
  data <- x$getMatrix()
  cache <- solve(data)
  x$setInverse(cache)
  
  cache
  
}
