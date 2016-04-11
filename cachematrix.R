## makeCacheMatrix and cacheSolve store the inverse of a matrix
## so that if called later, it will check the cache first before computing it

##makeCacheMatrix creates four functions that once called upon will cache the 
##inverse of a matrix to be referenced by cacheSolve 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(calcinv) m <<- calcinv
  getmatrix <- function() m
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

## cacheSolve first checks the cache stored by makeCacheMatrix to see if a value has been stored in m
  ## if a value is found, it will simply return the value in the cache
  ## if not found, it will compute the inverse of the new matrix

cacheSolve <- function(x, ...) {
  m <- x$getmatrix
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
## Return a matrix that is the inverse of 'x' 
  data <- x$get
  m <- solve(data)
  x$setmatrix(m)
  m
}