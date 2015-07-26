## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. 
## The following pair of functions can be used to cache the inverse of a matrix.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##  cacheSolve function computes the inverse of the special "matrix" returned 
##  by makeCacheMatrix above. If the inverse has already been calculated 
##  (and the matrix has not changed), then cacheSolve will retrieve 
##  the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("Cached data is available.")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinverse(inv)
  inv
}

## Testing
##  > x <- matrix(cbind(1:2,3:4), nrow=2, ncol=2)
##  > source('cachematrix.R')
##  > m <- makeCacheMatrix(x)
##  > m$get()
##  [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4
##  > cacheSolve(m)
##  [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##  > cacheSolve(m)
##  Cached data is available.
##  [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##  > 