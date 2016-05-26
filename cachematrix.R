## This program convert a matrix into a structure that contains subfuntions that
## interact with the inverse of the matrix.

## MakeCacheMatrix converts into a structure that contains the matrix itself, it
## s inverse which is initialize as NULL
## and 4 functions namely:

##      1. setMatrix: set the matrix to a new one and reset the inverse to NULL. 
##      This is to tell cacheSolve that the inverse of the new Matrix is not yet
##      computed and hence cacheSolve would go ahead to find it.

##      2. getMatrix: return the matrix itself

##      3. setInverse: assign a result to the inverse matrix (this is done without
##      the actual computing the inverse since it is assumed that the inverse is
##      computed correctly

##      4. getInverse: return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  
  setInverse <- function(inverseMatrix) inv <<- inverseMatrix
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve check if the inverse of the matrix has been "cached". If so it 
## returns the cached result. Otherwise it goes on to compute the inverse
## matrix through R-built in solve function. It saves the inverse in the
## cache so subsequently user can just get it straight from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data);
  x$setInverse(inv)
  inv
}