##Below function take matrix as input and then does the inversion of the input matrix and then finally stores it in 
##the cache.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##The following function derives the inverse of the square matrix created with the above function.
##However, it first checks to see if the above solution is already available.
##If so, it gets the solution from the cache and skips the computation.
##Otherwise, it does the inversion of matrix and sets the new derived matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
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
