## these functions allow a "matrix" object to be created that can cache
## the value of its inverse so that it doesn't need to be recalculated
## every time it is used unless the matrix changes


## creates a "matrix" object that is really a list containing funtions
## to get and set the value of the matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    
  
  #function that sets matrix to a new value
  set <- function(y) {
    x <<- y
    inv <<- NULL     #set cached inverse to NULL when matrix changed
  }
  
  #function that returns current matrix
  get <- function() x
  
  #function that sets inverse to a new value
  setinverse <- function(inverse) inv <<- inverse
  
  #function that returns current cached inverse
  getinverse <- function() inv
  
  #return list containing the functions for this "matrix" object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## returns a matrix that is the inverse of x, where x is a "matrix" object 
## created by makeCacheMatrix

cacheSolveNumeric <- function(x, ...) {
  #get cached inverse 
  inv <- x$getinverse()   
  
  #check for cached data and return it if not NULL
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #get matrix data and calculate inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  #cache newly calculated inverse and return it
  x$setinverse(inv)
  inv
}