## The functions below are to return the inverse of a matrix. If the 
##inverse already exists in the cache, the result is fetched from the ##cache. 
##Else it is calculated. 

## Function makeCacheMatrix is the construct the special "Matrix" with the getters and setters. 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  	set <- function(y){
    x <<- y
    m <<- NULL
  	}
  	get <- function() x
  	setInverse <- function(inverse) m <<- inverse
  	getInverse <- function() m
  	list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function cacheSolve is to compute the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
  	if(!is.null(m)) {
    message("getting cached data")
    return(m)
  	}
  	data <- x$get()
  	m <- solve(data, ...)
  	x$setInverse(m)
  	m

}
##end 
