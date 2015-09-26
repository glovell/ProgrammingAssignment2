
## The following code creates an enhanced matrix object that stores
## the value of a square matrix and its inverse if the inverse has been 
## computed.  This enhanced matrix object is created by evaluating the 
## function makeCacheMatrix.  The value of the matrix inverse
## can be computed and set in this object by the function cacheSolve.
## cacheSolve will modify this enhanced matrix object if and only if
## this object does not already have the value of the matrix inverse.

## The function makeCacheMatrix outputs an enhanced matrix object based 
## on the square matrix x.
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) minv <<- inv
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
## This function cacheSolve takes as input the special matrix object created by
## evaluating the function makeCacheMatrix.  It modifies this object
## if necessary by setting the value of the matrix inverse if this value has
## not been cached.  This function also returns the computed or cached 
## value of the inverse. 
cacheSolve <- function(x, ...) {
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  mat <- x$get()
  minv <- solve(mat, ...)
  x$setinv(minv)
  minv
}