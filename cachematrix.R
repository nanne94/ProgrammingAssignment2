## The following two functions will create the inverse of
## a matrix, cache it in a special object, compute it
## and retrieve it on demand.

## The first function is called makeCacheMatrix: This function creates
## a special "matrix" object that can cache its inverse. The returned object
## is a list of functions.

makeCacheMatrix <- function(storedMatrix = matrix()) {
  cachedInverse <- NULL
  
  ## Replaces the matrix stored in this object by the new matrix
  ## in the first argument 'y'.
  set <- function(y) {
    storedMatrix <<- y
    cachedInverse <<- NULL
  }
  
  get <- function() {
    storedMatrix
  }
  
  setinverse <- function(inv) {
    cachedInverse <<- inv
  }
  
  getinverse<-function() {
    cachedInverse
  }
  
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## The second function is called cacheSolve: This function computes
## the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not
## changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinverse(m)
  
  m
}
