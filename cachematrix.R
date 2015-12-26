## The following two functions use caching to enable fast operation of 
## computing the inverse of a special matrix.



makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its
    ## inverse
    ##
    ## Args:
    ##   x: A matrix which is assumed be invertible
    ##
    ## Returns:
    ##   A special "matrix" object
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)

}




cacheSolve <- function(x, ...) {
    ## This function computes the inverse of the special "matrix" returned by 
    ## makeCacheMatrix function. The function retrieves the inverse from the 
    ## cache if the inverse was already calculated and the matrix has not
    ## changed.
    ##
    ## Args:
    ##   x: A special "matrix" object
    ##
    ## Returns a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
