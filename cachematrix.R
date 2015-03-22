## There are two functions here, togather they create a matrix and cache the inverse of the matrix.
## Jeez I'm really sucked at english and I'm staying up late to get this assignment done, 
## so hope you get what I mean.

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y # assigning y to x which is not in the same environment as y, so <<- is required.
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) m <<- inverse 
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
  # this function returns a list, hence in cacheSolve, $ is required when subsetting functions

}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() # get the inversed matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if m is null, execute code below to get the inversed matrix.
  data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
  m #returning the inversed matrix
}
