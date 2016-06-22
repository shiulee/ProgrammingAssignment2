## In order to save time and computing, these functions used together
## cache the inverse of a matrix so it does not have to be computed more than once.

## makeCacheMatrix is a function that creates special "matrix" that can cache
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## cacheSolve is a function that computes the inverse of the "matrix" returned from
## makeCacheMatrix and returns this inverse. If this inverse has already been solved,
## then cacheSolve retrieves this inverse from the cache instead of computing it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
