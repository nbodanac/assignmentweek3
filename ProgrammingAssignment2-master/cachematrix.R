## Put comments here that give an overall description of what your
## functions do

## The following functions create an object that stores a matrix and caches its inverse. 
#The first function, makeCacheMatrix creates a  matrix, which sets the value of the matrix, gets the value, then sets the
#inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the  matrix returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}