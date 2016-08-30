
##Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse. makeCacheMatrix contains 4 functions: set, get, setmean, getmean.
## - get is a function that returns the vector x stored in the main function.
## - set is a function that changes the vector stored in the main function.
## - setmean and getmean are functions very similar to set and get.
## - They don't calculate the mean, they simply store the value of the input in a variable m.
## - into the main function makeVector (setmean) and return it (getmean)
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function "cacheSolve" computes the inverse of the special "matrix" 
## (which is the input of cachemean) returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache. 
## If the inverse has not been calculated, data gets the matrix stored 
## with makeCacheMatrix,m calculates the inverse, and x$setmean(m) 
## stores it in the object m in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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