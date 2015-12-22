## Work with cached matrix objects
## The objects store the results of calculations in memory for reuse.

## Create a cached matrix object out of a regular matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() { x }
  setSolve <- function(solution) { s <<- solution }
  getSolve <- function() { s }
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Find the inverse of a cached matrix. Will return cached result if already
## called before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  s <- x$getSolve()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}
