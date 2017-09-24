## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# cached matrix inverse
  minv <- NULL
  
  ## get and set for matrix
  mget <- function() x
  mset <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  ## getter/setter for matrix inverse
  getinv <- function() minv
  setinv <- function(inverse) 
  minv <<- inverse
  
  ## return list of functions for matrix
  list(mget=mget, mset=mset, getinv=getinv, setinv=setinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	minv <- x$getinv()

  # return cached matrix inverse if it's been already computed
  if (!is.null(minv)) {
    message("inverse is cached")
    return(minv)
  }
  
  # compute inverse of matrix 
  m <- x$mget()
  minv <- solve(m, ...)
  
  # caching inverse
  x$setinv(minv)
  
  # return - inverse of matrix
  return(minv)

}
