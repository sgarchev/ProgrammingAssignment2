# The function makeCacheMatrix create a special kind of matrix object that can cache its inverse
# and return a list of function
makeCacheMatrix <- function(x = matrix()) {
  # Initializing the property for store the cached inverse matrix
  inv <- NULL
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse of matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Method to get the inverse of matrix
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function cacheSolve computes the inverse of the matrix, which returned by the "makeCacheMatrix" function
cacheSolve <- function(x, ...) {

  inv <- x$getInverse()
  
  # If inv is not NULL
  if(!is.null(inv)) {
    # return the value of m with a message
    message("getting cached data")
    return(inv)
  }
  # If inv is not calculated, then calculate it
  data <- x$get()
  inv <- solve(data, ...)
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
