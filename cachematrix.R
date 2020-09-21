
## Caching the Inverse of a Matrix
## When the inverse of the matrix is cached, and the contents of the matrix are same
## It does not recompute the inverse of the matrix, instead in checks in the cache first
## And give the results accordingly.
## In the following code there are pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()){
  invs <- NULL
  set <- function(y){                    ## Setting the value of the matrix
    x <<- y
    invs <<- NULL
  }
  get <- function() {x}                 ## Getting value of the matrix
  setInverse <- function(inverse)       ## Setting value of the inverse
  {
    invs <<- inverse
  }
  getInverse <- function() {invs}       ## Getting value of the inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special 'matrix' created by 
## above. If the inverse has already been calculated , and the 
## matrix has not changed, then it should give the inverse from the cache.




cacheSolve <- function(x, ...){
  invs <- x$getInverse()             ## Gets inverse of 'x' and assign to invs
  if(!is.null(invs)){                ## checking whether the inverse has already been calculated
    message("getting cached data")
    return(invs)
  }
  mat <- x$get()                     ## If not, then getting the inverse by computing
  invs <- solve(mat, ...)
  x$setInverse(invs)
  invs
}
