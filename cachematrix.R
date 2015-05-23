## Put comments here that give an overall description of what your
## functions do

## Following function set or initialize.
## When you call this function first it initialize
## out of those x and m are set and can be accessed.
## Those can be accessed by x$get() and x$getinverse() respectively.
### Initially those values are set to NULL before calling cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  
  print("makeCacheMatrix...")
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 

}


## Following function computes the inverse first time it's called.
## the subsequent calls to this function returns what's in cache
#getInverse() is to return from the cache
# solve(matrix) returns the inverse of the matrix,
# provided the matrix should be non singular.
#non singular matrix by definition, it's determinant is not 0.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  print ("cachesolve ...")
  m<-x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
  
  
}
