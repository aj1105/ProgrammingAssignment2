## function to set & get input matrix and to set inverse and get inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
  x <<- y
  i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function will take input from the above function i.e. from " x$get " and it return the inverse of the matrix
## if the inverse is already stored then it will return that instead of recalcucalting it.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {                    ##will return stored value(if any)
    message("getting cached data")
    return(i)
  }
  data <- x$get()           ##data is assigned as x$get() which was stored in makeMatrix func.
  i <- solve(data, ...)     ##inverse is calculated for "data" and stored in i
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
