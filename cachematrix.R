## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix should create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x 
  setInverse <- function() inv <<- solve(x)
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve's goal is to compute the inverse of the special "matrix" returned by the function makeCacheMatrix above. If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtrx <- x$getInverse()
  
  if(!is.null(mtrx)){
    message("Getting cached data...")
    return(mtrx)
  }
  data <- x$get()
  mtrx <- solve(data)
  
  x$setInverse(mtrx)
  
  mtrx
  
}
#end of program
