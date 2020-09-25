## makeCacheMatrix is a function that will create a special matrix object to cache its inverse
## cacheSolve computes inverse of special matrix returned by the above makeCacheMatrix function
## if the inverse of matrix has been calculated and has not changes then it retrieves from cache

## create special matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y){
      x <<- y
      j <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) j <<- inverse
    getInverse <- function() j 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
  }


## retrieve inverse 

cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("retrieve inverse from cache")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}