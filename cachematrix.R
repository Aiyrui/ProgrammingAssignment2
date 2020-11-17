##The function makeCacheMatrix creates a special "matrix" object and
## caches its inverse. The cacheSolve computes the inverse of the special
## matrix object returned by the makeCacheMatrix function. However, it first
## looks to see if the inverse has already been solved. If already computed,
## cacheSolve retreives the inverse from the cache. Otherwise, the cacheSolve
## calculates for the inverse.

## The following function creates a special "matrix" object by setting 
## and getting the values of the matrix as well as its inverse, then store
## the inverse in the cache.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  matrix <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(matrix = matrix, get = get,
       setInverse = setInverse, getInverse = getInverse)
}


## The following function calculates the inverse of the special matrix returned
## by makeCacheMatrix. If the inverse has already been computed, it retrieves
## the inverse from the cache. Otherwise, it computes the inverse and set it
## via the setInverse function.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i

}
