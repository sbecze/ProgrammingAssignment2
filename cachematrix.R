## Make a closure to store the matrix, it's inverse 
## and appropriate functions to maniplate the them.
##


# This function creates a special "matrix" object that can cache its inverse.
# Elements: x - the given matrix
#           inv - inverse of x
#           set(y) - sets the x
#           get() - retuns x
#           setinv(i) - sets the inverse matrix
#           getinv() - returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated then 
# it retrieves the inverse from the cache, otherwise 
# it gets it from cache and returns it.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("Get cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  message("Set data in cahce")
  x$setinv(i)
  i
}
