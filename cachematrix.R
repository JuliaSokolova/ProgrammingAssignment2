
## This pair of functions cache the inverse of a matrix.



## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setcach <- function(cash) n <<- cash
  getcach <- function() n
  list(get = get,
       setcach = setcach,
       getcach = getcach)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  n <- x$getcach()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setcach(n)
  n
}
