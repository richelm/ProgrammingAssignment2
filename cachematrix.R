## FILE: cachematric.R
## DATE: March 21, 2015
## 
## INTRODUCTION
## ------------
## Computing the inverse of a matrix can be potentially time consuming. If 
## you have to compute the inverse of an unchanging matrix repeatedly, say
## in a loop, then this leads to unnecessary computations. 
##
## To make the computation of the inverse of a matrix more efficient, we
## make use of R scoping rules to cache the inverse of a matrix. This
## script provides two functions to cache and solve the inverse of a 
## matrix. R function solve is used to calculate the inverse of a 
## matrix. It is assumed that the matrix supplied is always invertible.
## 
##
## FUNCTION: makeCacheMatrix
## -------------------------
## This function creates a special object that is a list containing a 
## function to:
##    1. Set the value of the matrix
##    2. Get the value of the matrix
##    3. Set (solve) the inverse of the matrix
##    4. Get the inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## FUNCTION: cacheSolve
## --------------------
## This function solves the inverse of the matrix created with function
## makeCacheMatrix. It first checks to determine if the inverse of the matrix
## has already been solved. If it has, it skips solving the inverse and returns
## the cached value. Otherwise, it solves the inverse and caches it via the 
## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
