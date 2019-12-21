## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function defines a matrix and its inverse, and is able to generate a cache to store the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function first checks to see whether the inverse of the matrix has been calculated and stored. If not, it is calculated. IN either case, the inverse of the matrix is returned (via the variable i).

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
