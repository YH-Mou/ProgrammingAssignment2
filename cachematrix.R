## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The first function, makeCacheMatrix creates a special "Matrix",
# which is really a list containing a function to
# 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix
# Remark. The input matrix is assumed to be invertible as suggested in the description of the assignment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# The following function calculates the inverse matrix of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse matrix has already been calculated. 
# If so, it gets the inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse matrix in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}