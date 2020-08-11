## Put comments here that give an overall description of what your
## functions do

## "makeCacheMatrix" function will take a matrix as input and computes with four 
## internal functions (set, get, getinverse, setinverse) and gives out a list

makeCacheMatrix <- function(x = matrix()) {
  inv <<- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()(x)
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## "cacheSolve" function will take a matrix as input and calculates the 
## inverse of matrix , then cache that value into main matrix,which is 
## created using "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
