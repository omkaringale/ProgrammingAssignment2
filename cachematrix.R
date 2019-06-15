## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix is used to store a matrix and cacheSolve is used to calculate its inverse.
## Write a short comment describing this function
##makeCacheMatrix is a list containing the objects set,get,setmatrix,getinverse which point to the functions of the same name which have been defined in the parent directory
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getinverse = getinverse)
}


## Write a short comment describing this function
##cacheSolve calculates the inverse of the matrix and if it already exists,just uses the one stored in memory instead of recalculating it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat)
  x$setmatrix(m)
  m
}

