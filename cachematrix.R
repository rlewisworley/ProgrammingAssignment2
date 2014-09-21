## These functions allow you to create a matrix object and calculate the inverse 
## If the inverse has already been calculated, it will use a cached value 
## instead of recalculating.

## makeCacheMatrix will return a special object containing a matrix that you've created.

## How to use this:  testMatrix<-makeCacheMatrix(matrix(rnorm(25),nrow=5,ncol=5))
## testMatrix$get()
## testMatrix$setInverse()
## testMatrix$getInverse()


makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) matrixInverse <<- solve
  getInverse <- function() matrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve will return a matrix that is the inverse of 'x'
## if the inverse has already been determined,
## it will use a cached value instead of recalculating it.
## It assumes you've already created an object using makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixInverse <- x$getInverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  data <- x$get()
  matrixInverse <- solve(data, ...)
  x$setInverse(matrixInverse)
  matrixInverse
}
