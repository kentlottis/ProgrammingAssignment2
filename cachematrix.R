## R Programming - Assignment 2 - Caching Matrix Inverse


## makeCacheMatrix() This function creates a special "matrix" object that can cache its inverse.
#
# Note: this implementation intentionally differs from the cacheMean example in that it omits a 'setInverse' method
#       instead, the getInverse() method itself is responsible for caching the inverse when it is first computed 
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL

    # the set() method sets the value of the matrix and clears the cached inverse
    # returns the new value of the matrix
    set <- function(value) {
      cachedInverse <<- NULL
      x <<- value
    }
    
    # the get() method returns the value of the matrix
    get <- function() {
      return(x)
    }
    
    # the getInverse() method returns the inverse of the matrix
    # if the inverse has previously been computed, the cached value is returned
    # otherwise, the inverse is computed and cached for future reference
    getInverse <- function() 
      {
        if (is.null(cachedInverse))
        {
          message("performing matrix inversion")
          cachedInverse <<- solve(x)
        }
        
        return(cachedInverse)
      }
    
    list (set = set, get = get, getInverse = getInverse)
}


## cacheSolve() - computes the inverse of the special "matrix" returned by makeCacheMatrix above
#
# Note: this implemenation differs from the cacheMean() example in the assignment in that it realized on the 
#       getInverse() method itself to compute and cache the inverse. This approach makes it possible to use
#       the "matrix" object in isolation without the need for the cacheSolve() helper method
#
cacheSolve <- function(x) {
  x$getInverse()
}
