## Put comments here that give an overall description of what your
## functions do

## create the special object cachematrix
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  ##function to set the matrix and store it in the object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##function to get the matrix
  get <- function() x
  ##function to cache the inverse
  setinverse <- function(minv) m <<- minv
  ##function to get the cached inverse (if exist)
  getinverse <- function() m
  ##function list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## function to compute the inverse of a matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## get cached inverse matrix (if exist)
  m <- x$getinverse()
  ## check if matrix inverse exist
  if(!is.null(m)) {
    ##ok exist, then return
    message("getting cached data")
    return(m)
  }
  ##if not, get the original matrix to perform the inverse
  data <- x$get()
  ## perform the inverse
  m <- solve(data, ...)
  ## cache the inverse matrix inside the special object
  x$setinverse(m)
  ## return inverse
  m
}
