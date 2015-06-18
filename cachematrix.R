# makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse
# The object saves the inverse
# Saves the matrix to variable x and its inverse to variable t
# Returned object (list) contains function to do the following:
# sets matrix and resets cached inverse
# returns matrix
# saves solve value
# returns cached inverse value

makeCacheMatrix <- function(x = matrix()) {
  t <- NULL
  set <- function(y) {## sets matrix and resets cached inverse
    x <<- y
    t <<- NULL
  }
  get <- function() {## returns the matrix
    x
  }
  setSolve <- function(solve) {## saves solved value
    t <<- solve
  }
  getSolve <- function() {## returns cached inverse value
    t
  }
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

## cacheSolve
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
# Function to get the inversed matrix from a special object created by makeCacheMatrix.
# Takes the object of that type as an argument 'x', checks if the inverse value is already
# cached, and if it is returns the cached value; if not, this function calculates the
# inverse for the matrix saved in the 'x', saves it into 'x' cache using method 'setSolve'
# and returns the result.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  t <- x$getSolve()
  if(!is.null(t)) {
    message("getting cached data")
    return(t)
  }
  data <- x$get()
  t <- solve(data, ...)
  x$setSolve(t)
  t
}
 
