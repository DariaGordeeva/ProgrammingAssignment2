## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes an inverse of the "matrix". 
## If it exists already, the function retrieves the inverse from the cache. 


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The functions checks, whether an inversed value of the matrix exists already. 
## If not, it is calculated. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()  
  ## getting from the cache
  if(!is.null(m)) {
    message("getting cached data - retrieving an inversed matrix!")
    return(m)
  }
  
  ##calculating
  data <- x$get()
  m <-solve(data, ...)
  x$setinverse (m)
  m
  
}
