## The two functions help to calculate the inverse of a matrix and store it in 
## the cache

## This function creates a special "matrix" object that can cache its inverse. 


makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y){
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) invmat <<- inv
  getinverse <- function() invmat
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  invmat <<- x$getinverse()
  if(!is.null(invmat)){
    message("Getting cached data")
    return(invmat)
  }
  mat <- x$get()
  invmat <- solve(mat, ...)
  x$setinverse(invmat)
  invmat
}
