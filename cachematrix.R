## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	set <- function(y)
	j <- NULL
	{
	x <<- y
	j <<- NULL
	}
	get <- function()x
	setInverse <- function(inverse) j <<- inverse
	getInverse <- function() j 
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function figure out the inverse of the matrix by makeCacheMatrix and should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j))
  {
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
