## Function caching the inverse of a matrix

## This function creates a matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  
          inv <- NULL
          set <- function(y) {
          x <<- y
          inv <<- NULL
} 
          get <- function() x
          setinv <- function(inverse) inv <<- inverse
          getinv <- function() inv
          list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}

## This function solves for the inverse of the matrix created above.
## If there is a cache copy and the matrix has not changed, it retrieves
##the inverse matrix stored.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
          inv <- x$getinv()
          if(!is.null(inv)) {
           message("getting cached data")
          return(inv)
  }
          data <- x$get()
          inv <- solve(data, ...)
          x$setinv(inv)
          inv
}
