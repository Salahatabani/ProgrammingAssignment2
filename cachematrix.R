## Some calculation need huge allocation of resouces. Inverting matrix, especially if it is big, is very
##costly. Caching can be useful to avoid repeated process. The operation can be calculated once, then
##retrieved whenever is needed. The objective of this exercise is make a function to calculate the invert of a matix
##and cache it. Or retrieve it was already cached. 


##The function create matrix object to be cached it's inverse
makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  set <- function(y) {
    ##Set matrix value
    x <<- y
    s <<- NULL
    ##initial value is NULL so function will not be called if the invert was already calculated
  }
  get <- function() x
  ##get the value of the x matrix
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##Function to retrieve the cached invert of the matrix, or calculate it and cache it if i was not already there
cacheSolve <- function(x, ...) {
 
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
    #if it was caculated before the cached result will be recalled
  }
  data <- x$get()
  s <- solve(data, ...)
  ##solve function inverts a matrix (if it's invertible)
  x$setsolve(s)
  ##the inverted matrix will be cached
  s
}
