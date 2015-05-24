## Cache the Inverse of a Matrix

## The function "makeCacheMatrix" creates a special "matrix" object that can cache 
##  its inverse. This function returns a list of 4 functions as output:
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the inverse of the matrix
## (4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {  ## changes the matrix stored in the main function
    x <<- y             ## <<- operator expands the scope of assignment to the parent environment.
    m <<- NULL          ## reset m to NULL every time when set is called
  }
  get <- function() x   ## returns the matrix x stored in the main function
  setinv <- function(invert) m <<- invert  ## set the inverse of the matrix
  getinv <- function() m                   ## returns the inverse of the matrix 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  ## output a list of four functions
}


## The function "cacheSolve" computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated (and the matrix
##  has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {    ## x refers to the makeCacheMatrix function above
  m <- x$getinv()
  if(!is.null(m)) {                 ## if the inverse of the matrix is cached
    message("getting cached data")
    return(m)                       ## retrieve the inverse from cache and skips computing
  }
  data <- x$get()
  m <- solve(data, ...)             ## calculate the inverse of the input matrix
  x$setinv(m)
  m 
}
