## cachematrix.R

## R functions for creating the special object which store numeric vector and cache's its mean.

## Inverted mtrix by creating a cache matrix

##makeCachematrix() to craete a matrix and cache the inverse
##cacheSolve() returns the inverse of matrix that is cached using makeCacahedmatrix, if the inverse is already calculated and cached then it directly retrives from cache.

makeCacheMatrix <- function(x = matrix()) {
  #variable is pointed to null
  cachedInverse <- NULL
  set <- function(y) {
    # <<- is used to assign the value to an onject
    x <<- y
    cachedInverse <<- NULL
  }
  
  #setter and getter for inverse matrix
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse of cache matrix

cacheSolve <- function(x, ...) {
  #Return a matrix that is the inverse of original matrix
  inverse <- x$getInverse()
  
  #check if inverse is already calculated
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  #calculate inverse if its not already calculated
  matrix.data <- x$get()
  inverse <- solve(matrix.data, ...)
  x$setInverse(inverse)
  return(inverse)
}

