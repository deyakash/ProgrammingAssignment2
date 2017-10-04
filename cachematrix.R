## Overall, these functions can cache a matrix and its inverse
## If a cached inverse is found, it returns it instead of calculating again

## This function creates a special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special matrix 
## returned by the makeCacheMatrix function. If the inverse has already
## been calculated, cachesolve will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(inv)
  inv
}
