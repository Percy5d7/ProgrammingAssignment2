## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) #calculate the inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#Example:
funs <- makeCacheMatrix()
funs$set(matrix(1:4, 2))
funs$get()
funs$setInverse()
funs$getInverse()

## Write a short comment describing this function
# The point is that x and inv are stored in the enclosing 
# environment of the set, get, setInverse, getInverse 
# functions. That means the environment within which 
# they were defined, i.e., the environment created by 
# makeCacheMatrix()



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}