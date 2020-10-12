## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The first function, makeCacheMatrix creates a special "Matrix", which is really a Matrix containing a function to

##set the value of the Matrix
##get the value of the Matrix
##set the value of the Inverse
##get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y;
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) i <<- Inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

##The following function calculates the Inverse of the special "Matrix" created with the above function. 
##However, it first checks to see if the Inverse has already been calculated. If so, it gets the Inverse from the cache and skips the computation. 
##Otherwise, it calculates the Inverse of the data and sets the value of the Inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
