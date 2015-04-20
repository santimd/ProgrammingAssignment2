## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a function that returns a list of functions.
## It stores a matrix and its cached inversed value.

makeCacheMatrix <- function(x = matrix()) {

  cache <- NULL
  
  # store a new matrix
  setMatrix <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  # return the stored matrix
  getMatrix <- function () { x }
  
  # cache (store) the argument
  setInverse <- function(problem) {
    cache <<- problem
  }
  
  # get the catched value
  getInverse <- function () { cache }
  
  # return a list where each element of the list is one of the defined functions before
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function

## This function calculates the inverse of a matrix created with 
## the method makeCacheMatrix

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
