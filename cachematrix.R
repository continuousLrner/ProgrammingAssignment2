## Put comments here that give an overall description of what your
## functions do

## This function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the cacheMatrix
## 4. get the value of the cacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCacheMatrix <- function(cacheMatrix) m <<- cacheMatrix
  getCacheMatrix <- function() m
  list(set = set, get = get,
       setCacheMatrix = setCacheMatrix,
       getCacheMatrix = getCacheMatrix)
}


## This function, cacheSolve calculates the inverse of a matrix, 
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the value 
## of the invers matrix in the cache via the setCacheMatrix function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getCacheMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  if (!is.square.matrix(data)) {
    message("Invalid data. Matrix must be a square matrix to calculate its inverse")
    return(m)
  }
  m <- solve(data, ...)
  x$setCacheMatrix(m)
  m
}
