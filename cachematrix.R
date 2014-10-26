## This functions calculate the inverse of a Matrix


## Create a special matrix to get inversed

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setresult <- function(result) m <<- result
  getresult <- function() m
  list(set = set, get = get,
       setresult = setresult,
       getresult = getresult)
}

## cacheSolve calculates the inverse of the matrix or provides its cache value.


cacheSolve <- function(x, ...) {
  m <- x$getresult()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setresult(m)
  m
}
