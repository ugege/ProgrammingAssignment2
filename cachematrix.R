## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  solve <- solve(x) 
  get <- function() x 
  setsolve <- function (solve) m <<- solve 
  getinverse <-  function() m 
  r <-list(set = set, get = get, setsolve = setsolve, getinverse = getinverse) 
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  m <- r$getinverse 
    if (!is.null(m)){
      message("getting cached data")  
      return(m)
    }
  data <- r$get() 
  m <- solve(data)
  r$setsolve(m) 
  m ## return the inverse
}
