## A method that creates an object with some build- functions that supports 
## saving certain information of the object. 
## The object is basically a matrix and this matrix is saved within the object.
## The object can also save the matrix's inverse. 
## There are functions to set and get both matrix and matrix inverse data. 

makeCacheMatrix <- function(X = matrix()) {
  XInv <- NULL
  set <- function(Y) {
    X <<- Y
    XInv <<- NULL
    XInv
  }
  get <- function() X
  setInv <- function(matrixInv) XInv <<- matrixInv
  getInv <- function() XInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}



## The art of inverting matrices can be quite costly so this functions makes caching
## of an matrix possible. THe input to the function is an object of type 'makeCacheMatrix'. 
## cacheSolve() computes the inverse of the matrix and saves this within the matrix object. 
## Unless, if the matrix object already has its inverse saved. Then cacheSolve() loads the saved
## inverse instead. 

cacheSolve <- function(X, ...) {
  XInv <- X$getInv()
  if(!is.null(XInv)) {
    message("getting cached data")
    return(XInv)
  }
  matrix <- X$get()
  XInv <- solve(matrix, ...)
  X$setInv(XInv)
  XInv
}

