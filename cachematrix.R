## This R script file contains two functions that allow us to cache inverse matrix calculation
## The first funtion makeCacheMatrix creates a "special matrix" (a list) with the functions get(), set() (to get and set the matrix) 
## and getinv() and setinv() to cache and recover the inverse matrix.
## The second function cacheSolve takes the "special matrix" (the list created with the first function)
## and returns the inverse of the matrix if cached or calculates it and then caches it and returns it at the end.

## The function makeCacheMatrix 
  ## First we initialize the cached inverse to NULL
  ## We define set() function to set the matrix and initialize cached inverse to NULL 
  ## We define get() to return the matrix
  ## We define setinv() to chache the inverse matrix
  ## We define getinv() to return the chached inverse matrix
  ## Finally we return a four objects list with functions get() and set() (to get and set the matrix) and functions setinv() and getinv() 
  ## (to get and set inverse)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve:
  ## First gets the chached value
  ## If that cached value is not NULL prints a message and returns the inverse matrix then quits the function
  ## If it did not return gets the matrix using x$get()
  ## Calculates the inverse matrix using solve() 
  ## Caches the inverse matrix using x$setinv(inv)
  ## and prints (and returns) it
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv        
}
