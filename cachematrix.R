## The makeCacheMatrix function creates special "matrix".
## 1- sets the value of the Matrix.
## 2 - it gets the value of the Matrix.
## 3 - it sets the value of the Inverse of Matrix.
## 4 - it gets the Value of the Inverse of the Matrix.


makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The following function checks if the inverse was calculated
## If it did it skips the calculation and retrieves the inverse from the cache
## If not it calculates the inverse of the special "Matrix" created by the function above.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}