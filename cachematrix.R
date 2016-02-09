## Caching-Inverse of a Matrix:
## Please find below the functions that stores a matrix and caches its inverse.

makeCacheMatrix <- function(z = matrix()) {
  inv <- NULL
  set <- function(y) {
    z <<- y
    inv <<- NULL
  }
  retrieve <- function() z
  storeinv <- function(inverse) inv <<- inverse
  retrieveinv <- function() inv
  list(set = set,
       retrieve = retrieve,
       storeinv = storeinv,
       retrieveinv = retrieveinv)
}


## This function computes the inverse of the matrix created by function above. If the inverse has already been calculated then it should retrieve the inverse from the cache.

cacheSolve <- function(z, ...) {
  ## Return a matrix that is the inverse of 'z'
  inv <- z$retrieveinv()
  if (!is.null(inv)) {
    message("fetching cached data")
    return(inv)
  }
  mat <- z$retrieve()
  inv <- solve(mat, ...)
  z$storeinv(inv)
  inv
}
