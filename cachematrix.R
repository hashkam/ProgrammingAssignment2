## Cache the inverse of a matrix so that it does not need to be calculated again if it has already been calculated

## Function to create a special vector which can store the matrix and its inverse
makeCacheMatrix <- function (x = matrix()){
  inv <- NULL
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinv <- function (inverse) inv <<- inverse
  getinv <- function () inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function to check get the inverse of a matrix if it is in the cache or to compute it and store it in the cache

cacheSolve <- function(matrix, ...){
  inv <- matrix$getinv()
  if(!is.null(inv)){
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- matrix$get()
  inv <- solve(data, ...)
  matrix$setinv(inv)
  inv
}
