## makeCacheMatrix creates a Cache where cacheSolve can 
## check if the matrix has already been solved

## Explained above, not sure how much I learned here since I
## literally just changed the names of the variables and the 
## functions that were performed on them

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## First checks if the matrix has already been solved. If 
## false, calculates it and then caches it.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
