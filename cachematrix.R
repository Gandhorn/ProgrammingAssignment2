## makeCacheMatrix is used to build a vector containing key functions regarding 
# the matrix in the cache. It will be called to check if an inverse has already 
# been calculated and if so, retrieve it to save time


makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    
    setinv <- function(inv) inv <<- inv
    
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve is to be used on "CacheMatrix" object. It is meant to inverse 
# matrix. It will first check if an inverse has already been calculated. If yes,
# then it will directly retrive it. If not, it will calculated it the 
#traditionnal way and store the value in cache for next time


cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
  
}
