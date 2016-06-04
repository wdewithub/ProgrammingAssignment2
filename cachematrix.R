## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse of the matrix 
   minv <- NULL
  #store a newly defined matrix (in the cachesolve function) in cache 
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  #read the value of a previously defined matrix from the cache
  get <- function() x
  #store the value of a newly inverted matrix into cache
  setinverse <- function(solve) minv <<- solve
  #read the value of a previously inverted matrix from the cache
  getinverse <- function() minv
  #pass on the value of the function makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Cachesolve: computes the inverse of the special matrix returned by makeCacheMatrix. If the inverse
# has been calculated already and the matrix has not changed since, it retrieves 
# the inverted matrix back from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     minv <- x$getinverse
     #if the inverse exists already in cache, go and get it from there
     if (!is.null(minv)) {
       message("reading inverted matrix from cache")
       return(minv)
     }
     #if there is no inverted matrix in cache, read matrix from cache
     data <- x$get()
     #calculate inverted matrix and store it in cache
     minv <- solve(data, ...)
     x$setinverse(minv)
     #show the inverted matrix
     minv     
}
