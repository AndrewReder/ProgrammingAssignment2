## The goal of these functions is to reduce processor work by caching the inverse
## of a given matrix in local memory for later use, rather than repeatedly 
## computing

## makeCacheMatrix takes a matrix x and provides a list of four functions:
## set and get, which push and retrieve x from the cache
## setinverse and getinverse, which do the same thing but with the inverse of x

makeCacheMatrix <- function(x = matrix()) {
  ## i is our cache inverse, starts off null
  i <- NULL
  set <- function(y) {
    ## if we have a new matrix, we store it and erase cached inverse
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


## cacheSolve is an operator for the list created in makeCacheMatrix
## it takes the result of passing a matrix through makeCacheMatrix,
## then checks to see whether a solution has been already cached
## if so, it retrieves the solution with a note that it has done so
## otherwise it uses the solve function, stores the computed inverse in the cache
## and returns the inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
        ## check for cached inverse, report if there
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
        ## compute inverse and send to cache
  i  
        ## Return a matrix that is the inverse of 'x'
}
