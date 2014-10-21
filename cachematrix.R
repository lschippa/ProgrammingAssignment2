## Put comments here that give an overall description of what your
## functions do
# These functions will compute the inverse of a matrix and
# store the results into memory.
# Whenever the matrix has already been calculated, the function
# will retrieve the result instead of computing

## Write a short comment describing this function
# makeCacheMatrix() will take a matrix as input and 
# return the list to be used as input in cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) 
    m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve() will take as input one of the lists
# generated from makeCacheMatrix(), and return the
# inverse of the matrix fed as input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
