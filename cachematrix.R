#The first function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      a <- NULL
      set <- function(y) {
            x <<- y
            a <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) a <<- inverse
      getinv <- function () a
      list(set = set, 
           get = get,
           setinv = setinv,
           getinv = getinv)
}

#The second function calculates the inverse of the matrix object created by the first function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      a <- x$getinv()
      if(!is.null(a)) {
            message("getting cached data")
            return(a)
      }
      data <- x$get()
      a <- solve(data, ...)
      x$setinv(a)
      a
}
