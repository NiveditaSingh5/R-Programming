## This function creates a special "matrix" object that can cache its inverse.
## Inverse will exist only when the matrix is square and non-singular

## makeCacheMatrix creates a special "matrix", which is really a list containing
## to set the value of matrix, get the value of matrix, set its inverse and get 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  if (is.square.matrix(x)==TRUE && det(x)!=0){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }
  else
    print("Inverse of matrix does not exist")
}

## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache


cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
      
