## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly (there are also alternatives to
## matrix inversion that we will not discuss here).
## The two functions "makeCacheMatrix" and "cacheSolve" here are used to cache the inverse of
## a matrix.

## The function "makeCacheMatrix" creates a special "matrix", which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m = NULL
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  get = function() x
  setMatrix = function(inverse) m <<- inverse
  getMatrix = function() m
  list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)

}


## The function "cacheSolve" calculates the inverse of the special "matrix." It first checks to see if
## the inverse has already been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in
## the cache via the function "setMatrix".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m = x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data = x$get()
  m = solve(data)
  x$setMatrix(m)
  m
}
