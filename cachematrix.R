
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## m will store the inverse value. initially m is set to NULL
  m<<-NULL
  set <- function(y) {
    x <<- y
    m<<-NULL
   }
  get <- function() x
  
  # inverse is computed using solve function
  inverse_val<-solve(x)
  setinverse <- function() m <<- inverse_val ## inverse value is stored in m
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  
  ## if nrow(m) is NULL that means the inverse is not cached.
  if(!is.null(nrow(m))) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  ## inverse is calculated and stored in the cache
  inverse_val<-solve(data)
  x$setinverse()
  inverse_val
}
