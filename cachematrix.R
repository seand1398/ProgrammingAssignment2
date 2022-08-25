
## This function takes a matrix and creates a list containing 4 functions. 
# get will return the matrix (x) provided to the outer function
# set will set the matrix (x) to (y) provided 
# setInv will set the inverse of the matrix (inv) to (i) provided
# getInv will return the value of inv 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  list(set = set, get = get,
       setInv= setInv,
       getInv = getInv)
}
# This function takes a list created by the makeCacheMatrix and checks if the original matrix already has an inverse cached. 
# If it does, it returns that inverse and and prints a message stating that it returned a cached value
# If not, it calculates the inverse, stores in in the list provided, and prints it to the console
# Taken together, these functions allow the user to store the matrix and its inverse in the function environment 
# and access them at any time 
cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInv(inv)
  inv
}
