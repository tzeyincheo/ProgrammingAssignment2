## The makeCacheMatrix function will first set and get 
## values of inverse of matrix, and have them saved in the cache. 
## The cacheSolve function will then check if the inverse of the given matrix
## is in cache,  and if not, it will compute using the solve function.

#The first function, `makeCacheMatrix, creates a special "matrix", 'x' 
#which is a matrix containing a function to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse of matrix
#4.  get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <- function (y){
    x<<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##the second function, cacheSolve, returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



