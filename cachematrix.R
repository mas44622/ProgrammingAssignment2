## This exercice is designed to cache the inverse of a matrix 
## in order to retrieve it later in a more efficient way. 

## The first step is to create the matrix that can cache
## it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<- function(y) 
    x<<- y
    inv<<- NULL

  ## The second step is to create the inverse of the function
  
    get <- function()x
  
    setInverse <- function(inverse)
    inv <<- inverse
  
    getInverse <- function()inv

    list(set=set, get=get, 
        setInverse=setInverse,
        getInverse=getInverse)
}

## The Third step is to return a matrix that is the inverse of x

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
