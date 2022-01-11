## This file contains 2 functions, makeCacheMatrix and cacheSolve, that cache the inverse of a matrix

## makeCacheMatrix: this function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: this function computes the inverse of the matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)) {
          message("getting my cached matrix")
          return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setInverse(m)
  m
}
