## This function creates a special "matrix" object that can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
#set <- function(y){
#x <<- y
#m <<- NULL}
solve <- solve(x) 
get <- function() x # a function that returns the matrix whose inverse is to be calculated
setsolve <- function (solve) m <<- solve 
getinverse <-  function() m ## a function that returns the calculated inverse
r <-list(get = get, setsolve = setsolve, getinverse = getinverse) 
r ## a list of the functions in makeCacheMatrix
}
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- r$getinverse ## assign the cached inverse of the matrix previously calculated to m
    if (!is.null(m)){
      message("getting cached data")  
      return(m)## if the value of m is not NULL return it as the inverse of x
    }
  data <- r$get() ## if the value of m is NULL get the matrix 
  m <- solve(data)## calculate the inverse of the matrix
  r$setsolve(m) 
  m ## return the inverse
}
