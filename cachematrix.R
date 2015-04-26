## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix stores a matrix X in memory 
## cacheSolve shows the inverse of a matrix if is in memory or computes the inverse and then shows the inverse 

## Write a short comment describing this function
## makeCacheMatrix uses scoping rules and stores matrices in memory 
## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix stores a matrix X in memory 
## cacheSolve shows the inverse of a matrix if is in memory or computes the inverse and then shows the inverse 

## Write a short comment describing this function
## makeCacheMatrix uses scoping rules and stores matrices in memory 
 
makeCacheMatrix <- function(x = matrix()) {
   
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
## cacheSolve uses corpcor, a library that avoids determinants and uses orthogonal descomposition 
## note: this function will try to load corpcor library and if it's not installed will try to install the library 
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
} 
