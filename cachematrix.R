## Author: Jérôme Picault

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a "special" matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  ## function to desencapsulate the matrix object contained in this special matrix object
  getMatrix <- function() x
  ## function to reset the cache if the value of the matrix changes
  setMatrix <- function(y){ 
    x <<- y
    inverse <<- NULL
  }
  list(setInverse = setInverse, getInverse = getInverse, 
       setMatrix=setMatrix, getMatrix=getMatrix)
}


## Write a short comment describing this function

## This function computes the inverse of the "special" matrix: 
## if the inverse has already been calculated (and the matrix has not changed)
## it returns the cached inverse
cacheSolve <- function(x, ...) {
  ## x is the special matrix to be inversed
  cacheValue <- x$getInverse()
  if (!is.null(cacheValue)){
    ## cached inverse existes
    message("getting cached data")
    return(cacheValue)
  }

  message("direct computation")
  ## here the cache value does not exist, we compute it and put it into the
  ## cache for future reuse
  # disencapsulation of the actual matrix to apply computation
  inverse <- solve(x$getMatrix(), ...)
  x$setInverse(inverse)
  ## return result
  inverse
}


## Tests the code above using a default matrix
testCode <- function(m = matrix(data = c(-3, 5, 6, -1, 2, 2, 1, -1, -1), nrow = 3, ncol=3)){
  ## "standard" inverse matrix
  m_inv <- solve(m)
  message("reference result:")
  print(m_inv)

  ## apply inversion twice to check that cache is used
  cacheMatrix <- makeCacheMatrix(m)
  print(cacheSolve(cacheMatrix))
  print(cacheSolve(cacheMatrix))
  print(cacheSolve(cacheMatrix))
}
