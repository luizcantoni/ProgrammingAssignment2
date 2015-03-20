## This function creates a kind of object that encapsulates an matrix passed as parameter.
## The created object could be used in a function that caches the inverted matrix of the
## encapsulated matrix.

## The returned list contain itens that are functions. This functions must to be used 
## to access the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function takes a makeCacheMatrix as parameter and calculates the inverse of
## the matrix presents in this object. Once calculated the inverse is cached and doesn't
## need to be calculated again.

## If the matrix could not be inverted the function throws an error.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <-tryCatch(solve(data),warning=message("This matrix cannot be invertible. See the error below."))
  x$setInverse(m)
  m  
}
