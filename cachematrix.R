## cachematrix.R
## by: Ellen Badgley
## R Programming
## March 2015

## These functions provide a "pseudo-object" that stores a matrix and caches its inverse as appropriate.

## Make the "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<-inverse  ## Sets the inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return the computed inverse of the "matrix" object.
## If present the cached inverse is returned, if not it is computed and returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse() 
  
  ## See if the inverse has already been cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If we get to here, the inverse was not cached and needs to be computed
  data <- x$get()
  m <- solve(data) ## The solve function with a single argument computes the inverse
  x$setinverse(m)
  m
}
