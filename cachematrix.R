## These functions create matrix objects for cacheing the inverse of a matrix,
## to avoid recalculating the inverse when the matrix data remains the same.

## makeCacheMatrix creates a makeCacheMatrix object containing a matrix and a
## list of functions in order to store the inverse matrix solution

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL  ## intialize inverse object
      
      set <- function(y) {
            x <<- y   ## reset value of x in parent environment
            s <<- NULL ## reset inverse in parent environment
      }
      
      get <- function() x  ## return x from parent environment
      
      setsolve <- function(solve) s <<- solve ##set s in parent environment
      
      getsolve <- function() s ## return s from parent environment
      
      list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
      ##returns named list of functions to parent environment
}


## cacheSolve takes an object of type makeCacheMatrix (matrix with functions)
## and returns the inverse. If the makeCacheMatrix object has not changed it
## returns the cached inverse, rather than recalculating.

cacheSolve <- function(x, y, ...) {
      s <- x$getsolve()  ##retrieve inverse matrix from input if there
      if (!is.null(s)) {
            message("Getting cached data!")
            return(s)
      }
      
      ##otherwise retrieve matrix and calculate, cache, and return inverse
      input.matrix <- x$get()
      s <- solve(input.matrix, y, ...)
      x$setsolve(s)
      s
}
