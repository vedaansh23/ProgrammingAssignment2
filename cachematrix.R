makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
  x <<- y  
  j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse)
          j <<- inverse  ##creating setInverse function
  getInverse <- function()
          j ##creating getInverse function
  list(set = set, get = get,
  setInverse = setInverse,
  getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data")
  return(j)## returning j
  }
  mat <- x$get()
  j <- solve(mat,...)##calling solve function
  x$setInverse(j)
  j
}
