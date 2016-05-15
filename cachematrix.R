## The following functions make and cache a matrix inverse so it does not have to be
## computed repeatedly

## makeCacheMatrix is a list of functions to set or get a matrix or its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}


## cacheSolve checks for a stored inverse then calculates an inverse if one does not exist

cacheSolve <- function(x, ...) {
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmat(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
