## Function to support cached (optimized) access to the inverse of a matrix

## Create a special "matrix object" that is capable of caching its inverse matrix

makeCacheMatrix <- function(matrix_base = matrix()) {

  # Initially, ensure that cached (inverse) matrix is undefined
  matrix_inv <- NULL

  # Function to (re)set the "base" matrix to the given matrix.
  # Clear any existing cached inverse matrix
  set <- function(matrix_new) {
    matrix_base <<- matrix_new
    matrix_inv <<- NULL
  }

  # Function to return current "base" matrix
  get <- function() {
    matrix_base
  }

  # Function to set cached inverse matrix to the given matrix
  setinverse <- function(new_inv_matrix) {
    matrix_inv <<- new_inv_matrix
  }
  
  # Function to return current cached inverse matrix (possibly NULL)
  getinverse <- function() {
    matrix_inv
  }
  
  # At end of "constructor", return a named list of 4 functions defined above
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Function to return inverse matrix of the matrix identified by the matrix object (function). 
## If cached copy exists, that will be returned.
## If not, inverse will be computed, cached, and returned.

cacheSolve <- function(matrix_obj, ...) {
  
  matrix_inv <- matrix_obj$getinverse()
  if (is.null(matrix_inv)) {
    # Inverse is not currently cached, so compute and cache it here
    matrix_inv <- solve(matrix_obj$get(), ...)
    matrix_obj$setinverse(matrix_inv)
  }
  matrix_inv

}

