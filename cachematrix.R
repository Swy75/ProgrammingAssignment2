## Pair of functions that :
# create a special "matrix" (with getters and setters) and cache the inverse of a matrix
# compute the inverse of a special "matrix" if it has not been calculated


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}


## Example :
myMatrix <- makeCacheMatrix(matrix(c(1,2,3,0,1,4,5,6,0),nrow = 3, ncol = 3, byrow = TRUE))
cacheSolve(myMatrix)
myMatrix$getinverse()
