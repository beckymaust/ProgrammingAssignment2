## This set of functions will compute the inverse of a matrix and then cache 
## the matrix and it's inverse to be called later. This will save computational
## time if the inverse needs to be used multiple times.

## creates a list of 4 functions, set which sets the value of the matrix, get which
## returns the value of the matrix, setinverse sets the value of the inverse,
## getinverse returns the cached value of the inverse

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


## cachesolve returns the value of the inverse of x
## if the inverse is already cached, it will retrieve it
## Otherwise, it will compute the inverse, cache it using setinverse
## and then return the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
