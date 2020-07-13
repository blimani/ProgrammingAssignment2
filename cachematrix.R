## These two functions create a special object which stores a matrix,
## and cache's it mean.

## makeCacheMatrix builds a set of functions. These functions set the
## matrix, and get the value of the matrix. Additionally, they also 
## get and set the value of the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve looks to retrieve a value for the inverse from the cache.
## If it cannot it do this, it solves for the inverse, caches it, 
## and prints it.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
        
}
