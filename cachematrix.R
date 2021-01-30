# This code demonstrates memoization in the calculation of the inverse of a 
# matrix. If the inverse of a matrix is already computed, then the cached
# result is returned, otherwise the inverse is computed and its value cached.

## makeCacheMatrix returns a list containing four functions:
##   set : set the value of the matrix,
##   get : get the value of the matrix,
##   setinverse : set the inverse of the matrix,
##   getinverse : get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created with the 
## makeCacheMatrix function. It first checks to see if the inverse matrix has
## already been calculated. If so, it returns the value from the cache and 
## skips the computation. Otherwise, it calculates the inverse matrix and sets
## the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
