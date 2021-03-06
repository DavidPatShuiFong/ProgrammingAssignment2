## Creates a special matrix which stores a matrix and its inverse 'makeCacheMatrix'
## When the matrix of the inverse is computed 'cacheSolve', the inverse is stored/cached
## for future use

## example:
## > m <- makeCacheMatrix(matrix(c(7,2,3,4),nrow=2))
## > cacheSolve(m)
##             [,1]       [,2]
## [1,]  0.18181818 -0.1363636
## [2,] -0.09090909  0.3181818

## Creates and returns a special matrix which stores matrix 'x' and also allows
## the storage of (but does not compute) the inverse of matrix 'x'
## cacheSolve() is used to compute the inverse of matrix stored in variable
## created by this function
makeCacheMatrix <- function(x = matrix()) {
  ## (matrix) -> (special function storing matrix and its inverse)

  ## $get returns the currently stored matrix
  ## $setinverse stores the computed inverse of the matrix (typically computed by function cacheSolve)
  ## $getinverse returns the stored inverse of matrix 'x'
  
  ## store inverse of matrix in matrix.inverse
  matrix.inverse <- NULL
  set <- function(y) {
    ## x is used to store the matrix
    x <<- y
    ## when x is re-defined, matrix.inverse is currently not defined
    matrix.inverse <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) matrix.inverse <<- inverse
  getinverse <- function() matrix.inverse
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## computes and returns the inverse of matrix 'x'
## 'x' is a special function providing storage for a matrix (and it inverse)
## created by function makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## (function created by makeCacheMatrix) -> matrix
  
  ## once the inverse has been calculated, the inverse is cached
  ## and is returned without being calculated again when
  ## cacheSolve(x) is called in future

  ## firstly retrieve the stored inverse (if any) from 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if no stored inverse available, then compute the inverse
  data <- x$get()
  ## compute inverse of matrix 'data'
  i <- solve(data)
  ## store inverse for future retrieval
  x$setinverse(i)
  i
}
