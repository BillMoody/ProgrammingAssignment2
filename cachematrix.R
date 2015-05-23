#!/usr/bin/Rscript
## Coursera R Assignment 2
## Pair of function demonstrating lexical scoe.

cached_inv_mtx <- NULL
cached_mtx <- NULL

## Assign matrix to a cache 
# I would rather have this function create the matrix 
# i.e., function(v,r) return matrix(v,r)
makeCacheMatrix <- function(x = matrix()) {
  cached_mtx <- x
}

## Solve (insvert) a matrix of it has been solved before use the cached value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if (! is.null(cached_inv_mtx) && identical(x,cached_mtx)) {
    message("Cached")
    return (cached_inv_mtx)
  } else {
    message("Solving")
    inv <- solve(x)
    cached_mtx <<- x
    cached_inv_mtx <<- inv
    return(inv)
  }
}

## Test values
m1 <- makeCacheMatrix(matrix(c(0,66,7,13),2))
cacheSolve(m1);

m2 <- makeCacheMatrix(matrix(c(0,66,7,13),2))
cacheSolve(m2);

m3 <- makeCacheMatrix(matrix(c(0,99,7,75),2))
cacheSolve(m3);

m4 <- makeCacheMatrix(matrix(c(0,66,7,13),2))
cacheSolve(m4);

