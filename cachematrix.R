## These 2 functions combine to allow you find the inverse of a matrix and store it
## in memory cache to allow for faster retrieval if same inverse is called for later.

## makeCacheMatrix creates an object containing get/set functions. setinverse and
## getinverse set or get the inverse of the matrix in i.  get and set are used to 
## retrieve and store the matrix in x.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve receives an argument of the makeCacheMatrix object.  It will return
## the inverse of the matrix in that object.  In doing so, it will determine if
## the inverse can be pulled from memory or must ber calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  #print(paste("cs i:", i))
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  #print(paste("data: ", data))
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
