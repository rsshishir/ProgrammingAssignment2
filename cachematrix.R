## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #m<<-matrix(nrow=nrow(x),ncol=ncol(x))
  m<<-NULL
  set <- function(y) {
    x <<- y
    m<<-NULL
    #m<<-matrix(nrow=nrow(x),ncol=ncol(x))
  }
  get <- function() x
  inverse_val<-solve(x)
  setinverse <- function() m <<- inverse_val
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(nrow(m))) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  inverse_val<-solve(data)
  x$setinverse()
  inverse_val
}
