## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse of the matrix 
  inv <- matrix(, nrow=nrow(x), ncol=ncol(x))
  #define where to cache a new defined matrix and clear any previously calculated matrix inverse 
  set <- function(y) {
    x <<- y
    inv <<- matrix(, nrow=nrow(x), ncol=ncol(x))
  }
  #read the value of a previously defined matrix from the cache
  get <- function() x
  #store the value of a newly inverted matrix into cache
  setinverse <- function(solve) inv <<- solve
  #read the value of a previously inverted matrix from the cache
  getinverse <- function() inv
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
     inv <- x$getinverse
     #if the inverse exists already in cache, go and get it from there
     if (!(all(is.na(inv)))) {
       message("reading inverted matrix from cache")
       return(inv)
     }
     #if there is no inverted matrix in cache, read matrix from cache
     data <- x$get()
     #calculate inverted matrix and store it in cache
     inv <- solve(data, ...)
     x$setinverse(inv)
     #show the inverted matrix
     inv     
}
