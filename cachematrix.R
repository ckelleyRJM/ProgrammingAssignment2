# The functions below creates caches the inverse of
# a matrix to reduce compuational costs when referring
# the data later.

makeCacheMatrix <- function(x = matrix()) {
     
     # Cache is NULL when no values are stored
     cache <- NULL
     
     # Store a matrix
     setMatrix <- function(y) {
          x <<- y
     # Assign new value to matrix and void cache
          cache <<- NULL
     }
     
     getMatrix <- function() 
          x # Returns the stored matrix
     cacheInverse <- function(solve) 
          cache <<- solve # Cache new argument
     getInv <- function()
          cache # Get cached data
     list(setMatrix = setMatrix, 
          getMatrix = getMatrix,
          cacheInverse = cacheInverse,
          getInv = getInv) # Print list of functions
}



# The function below computes the inverse of a matrix
# returned from makeCacheMatrix. If available, the
# cached values will be returned. If not, they will
# be cached for future use.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getInv()
     if(!is.null(inv)){  
          message("Retrieving cached data...")
          return (inv)
     }
     data <- x$getMatrix()
     inv <- solve(data,...)
     x$cacheInv(inv)
     inv
}
