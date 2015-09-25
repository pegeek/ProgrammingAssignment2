## A pair of functions that enable caching the inverse of a matrix, a possibly
## costly computation, and retrieved the cashed value as long as the matrix has
## not changed


## This function creates a "matrix" object that can cache its inverse.
## The interface to the values are provided with get and set functions,
## both for the matrix and it's inverse.If the value of the matrix is
## changes, the function set wills et the inverse value to NULL, assuring
## that a stale value would not be returned.

makeCacheMatrix <- function(mtx = matrix()) {
      invmtx <- NULL
      set <- function(y) {
            mtx <<- y
            invmtx <<- NULL
      }
      get <- function() mtx
      setinverse <- function(x) invmtx <<- x
      getinverse <- function() invmtx
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the "matrix" object returned by 
## makeCacheMatrix function.If the inverse of a specific matrix object is 
## already computed, then the cashed value is retrieved from the cash. Otherwise, 
## the inverse if calulated and passed to the matrix object.

cacheSolve <- function(x, ...) {
      invmat <- x$getinverse()
      if(!is.null(invmat)) {
            message("getting cached inverse")
            return(invmat)
      }
      mat <- x$get()
      invmat <- solve(mat)
      x$setinverse(mat)
      message("Computing the inverse")
      invmat
      ## Return a matrix that is the inverse of 'x'
}
