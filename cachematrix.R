## This pair of functions will take in (or create) a matrix, x,
## establish a cache, then check the cache before calculating
## the matrix inverse and updating the cache if necessary.

## This function creates a matrix, a cache, and a way to change
## them using a list of four functions
makeCacheMatrix <- function(x = matrix()) {
     
     mat <- NULL
     set <- function(y) { ## set passes argument to matrix x
          x <<- y
          mat <<- NULL  ## ... and clears cache
     }
     
     get <- function() x   ## return matrix to invert
     setinverse <- function(inverse) mat <<- inverse ## write new inverse to cache
     getinverse <- function() mat ## return cache
     
     list(set = set, get = get,   ## list of functions
          setinverse = setinverse,
          getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x' either from the cache
## (if available) or by calculating it (in which case it also updates cache)
cacheSolve <- function(x, ...) {
     
     mat <- x$getinverse() ## load value from cache
     
     if(!is.null(mat)) {  # check if cache is null
          message("getting cached data") 
          return(mat)   ## if not, return the value in the cache, ending the function
     }
     
     data <- x$get()  ## if so, function procedes by retrieving matrix x
     mat <- solve(data, ...) ## finding its inverse
     x$setinverse(mat) ## and writing it to the cache.
     
     mat  
}
