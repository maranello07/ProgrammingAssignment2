## These functions are used to save computational time by using the cache, 
#so that when we need it again, it can be looked up in the cache 
#rather than recomputed

## creates a special matrix that contains a function to set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse function() m
    list(set = set, get = get,
         setinverse = setinverse
         getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned 
## If the inverse has already been calculated the inverse is retrieved from the cache

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
