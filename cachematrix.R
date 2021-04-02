## The function is able to cache the inverse of a matrix 

# The first function, makeMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeMatrix <- function(x = matrix()) {

  inv_ <- NULL
  set <- function( matrix ) {
    m <<- matrix
    inv_ <<- NULL
  }
  
  get <- function() m
  
  setinverse <- function(inverse) inv_ <<- inverse
  getinverse <- function() inv_
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

# The following function calculates the inverse of the special "matrix" created with the aboce 
# function. However, it first checks to see if the iverse has already been calculated. 
# If so, it gets the iverse from the cache and skips the computation. Otherwise, it calculates 
# the inverse of the matrix and sets the value of the inverse in the cache via the setiverse function.

cacheMatrix <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data) 
  x$setinverse(m)
  m
}
