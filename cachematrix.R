## This function creates a matrix that is suitable for caching

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the cache matrix
  mtrx <- NULL
  ## Internal method/function to re-initialize the cache matrix
  set <- function(y) {
    x <<- y
    mtrx  <<- NULL
  }
  ## Internal method/function to obtain the cache matrix
  get <- function() x
  ## Internal method/function to store the inversed matrix
  setinverse <- function(inversed) mtrx <<- inversed
  ## Internal method/function to obtain the inversed matrix
  getinverse <- function() mtrx
  ## Return a list with inversion-related functions that can be applied to a matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), then this function 
## retrieves the inverse from the cache.

cacheSolve <- function(x) {
## Obtain the inversed matrix
  inv <- x$getinverse()
## If the inversed matrix exists in cache, return cached data
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
## If the inversed matrix was not cached, calculate and store the inverse
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
## Return a matrix that is the inverse of 'x'
  inv
}
