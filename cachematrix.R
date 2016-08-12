## Put comments here that give an overall description of what your
## functions do


#~~~~~~~~~~~~~~~~~~~~~~~~~~  makeCacheMatrix ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matrix_inverse <<- inverse
  getInverse <- function() matrix_inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~ cachesolve ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# cacheSolve computes the inverse of the special "matrix" created by 
# makeCacheMatrix. 
#If the inverse has already been calculated and there has been no change to
#the matrix, then the inverse will be retrieved from the cache and the
#computation skipped.



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrix_inverse <- x$getInverse()
  if (!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  mat <- x$get()
  matrix_inverse <- solve(mat, ...)
  x$setInverse(matrix_inverse)
  matrix_inverse
}

#############~~~~~~FUNCTION EXAMPLES BELOW~~~~~~~################


#matrix_test <- makeCacheMatrix(matrix(1:4, 2, 2))
#matrix_test$get()
#cacheSolve(matrix_test)
