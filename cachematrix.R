
## The function makeCacheMatrix creates a special matrix object that can
## cache its inverse
## For this function I input 'x' as a matrix and set the solve value as 'k'
##  and every reference to 'mean' i changed to 'inverse'

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y){
    x <<- y
    k <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) k <<- inverse
  getInverse <- function() k 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## The function cacheSolve computes the inverse of the special matrix
## returned by makecache
## I changed the 'menu'to 'solve'
cacheSolve <- function(x, ...) {
  k <- x$getInverse()
  if(!is.null(k)){
    message("getting cached data")
    return(k)
  }
  mat <- x$get()
  k <- solve(mat,...)
  x$setInverse(k)
  k
}
