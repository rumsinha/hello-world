# Two functions created in R
# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#    If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


## calculates the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  #calculates the matrix inverse
  setMatInverse <- function(solve) matinv <<- solve
  getMatInverse <- function() matinv
  list(set = set,
       get = get,
       setMatInverse = setMatInverse,
       getMatInverse = getMatInverse)
}


## function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matinv <- x$getMatInverse()
  if(!is.null(matinv)) {
    message("getting cached data.")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data)
  x$setMatInverse(matinv)
  matinv		
}
