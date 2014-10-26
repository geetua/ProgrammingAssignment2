## Creates a special vector that is a list of functions 
## that are setters and getters for the matrix and its inverse
## It serves as a cache that holds the matrix and its inverse
## value
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that does a look up of cached matrix inverse
## if inverse is available in cache, it returns the cached value
## o/w it calculates the matrix inverse, saves it to cache and 
## returns the newly computed inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
	
}
