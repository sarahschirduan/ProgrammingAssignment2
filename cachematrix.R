# The purpose of these two functions, makeCacheMatrix & cacheSolve is to cache
# and return the inverse of a matrix


# The makeCacheMatrix function creates an R object that stores a matrix & its inverse

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

# The cacheSolve function requires an argument that is returned by makeCacheMatrix() 
# in order to retrieve the inverse from the cached value stored in the makeCacheMatrix() 
# object's environment.

# This function is necessary because the first function does not result in returning
# the inverse, it merely stores it 

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


#Testing functions
testmatrix <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)  
testmatrix
testmatrix2 <- makeCacheMatrix(testmatrix)
cacheSolve(testmatrix2)