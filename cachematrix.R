## The first function, makeVector creates a special "vector",
# which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverted matrix
# 4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmean <- function(inversse) inv <<- inverse
  getmean <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# The following function calculates the inverse of the special "vector" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
# from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets
#the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
 inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
