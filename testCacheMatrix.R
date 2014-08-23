## test the makeCacheMatrix() and cacheSolve() functions


testCacheMatrix <- function()
{
  a = makeCacheMatrix()

  # make sure all the methods are there
  stopifnot(!is.null(a$get))
  stopifnot(!is.null(a$set))
  stopifnot(!is.null(a$getInverse))

  # get after set works
  expected = 52
  a$set(expected)
  stopifnot(a$get() == expected)
  stopifnot(a$getInverse() == 1/expected)
  
  # set() resets cached inverse
  expected = 31
  a$set(expected)
  stopifnot(a$get() == expected)
  stopifnot(a$getInverse() == 1/expected)
  
  # set()/get() work for matrices
  expected = matrix(rnorm(16),4,4)
  a$set(expected)
  actual = a$get()
  stopifnot(dim(actual)==dim(expected))
  stopifnot(actual==expected)
  stopifnot(a$getInverse() == solve(expected))
  
  ##-----------------------
  # now test cacheSolve
  pain = matrix(rnorm(100),10,10)
  meds = solve(pain)
  a$set(pain)
  stopifnot(meds == a$getInverse())
  stopifnot(!is.null(cacheSolve(a)))
  stopifnot(meds == cacheSolve(a))
  
  #all of the above, but with a starting value
  z36 = matrix(rnorm(36),6,6)
  b = makeCacheMatrix(z36)
  stopifnot(b$get() == z36)
  stopifnot(b$getInverse() == solve(z36))
  
  # pass if we make it this far with no errors
  print("Pass!")
}

testCacheMatrix()
