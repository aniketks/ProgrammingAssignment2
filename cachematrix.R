## makeCacheMatrix creates a list where values about the matrix can be updated using cacheSolve

## makeCacheMatrix creates a list made up of four constructor functions. cacheSolve can then use get, inverse the matrix, and set the value of the inverse matrix for caching later

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function () i
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve retreives the inverse matrix, if it exists. If it doesn't, the function calculated the inverse and stores it in the variable for later retrieval. 

cacheSolve <- function(x, ...) {
       i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data") 
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
