## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  print("1")
  inv <- x$getinv()
  print("1")
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  print("1")
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
