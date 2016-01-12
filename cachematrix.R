## These functions help with computing the inverse of matrices and caching the result

## makeCacheMatrix returns a vector consisting of four functions: getting and setting
## matrix data, and getting and setting the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the cached inverse of the special matrix created with makeCacheMatrix or
## if the cached inverse is missing then computes and caches it

cacheSolve <- function(x) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
