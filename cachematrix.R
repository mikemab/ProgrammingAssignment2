## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function stores both the original and inverse of a matrix, with the inverse being calculated by the 
#cacheSolve function and stored with method setinverse, and can be gotten out of cache with getinverse
#to set a new matrix use the set method
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y){
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

#This function checks whether the cacheVector already stored an inverse matrix, if not it solves for its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
