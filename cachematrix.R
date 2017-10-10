## cachematrix.R

## Descriptions:

## function #1: makeCacheMatrix() - This function creates a special "matrix" object that can cache its inverse.

## function #2: cacheSolve() - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix() is a function that creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  ## initialize cachedMatrix variable
  matrixValue <- NULL
  ## function to set the matrix value.
  setMatrix <- function(y) {
    x <<- y
    matrixValue <<- NULL
  }
  ## funtion to get the matrix value.
  getMatrix <- function() {
    x
  }
  ## set the inverse of the matrix value.
  setMatrixInverse <- function(inverse) {
    matrixValue <<- solve(x)
  }
  ## get the inverse of the matrix value.
  getMatrixInverse <- function() {
    matrixValue
  }
  # special matrix list
  list(setMatrix = setMatrix, getMatrix = getMatrix, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}

## cacheSolve() is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixValue <- x$getMatrixInverse()

  if(!is.null(matrixValue)) {
    return(matrixValue)
  } else {
    matrixValue <- solve(x$getMatrix())
    x$setMatrixInverse(matrixValue)
    return(matrixValue)
  }
}
