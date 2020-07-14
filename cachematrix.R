## This pair of functions cache the inverse of a Matrix with the goal of avoid
## its repeated calculation


## This function generates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv_M <- NULL
    set <- function(y) {
        x <<- y
        inv_M <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inv_M <<- inverse
    getInverse <- function() inv_M
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function returns the inverse of the special matrix created in the previous
## function, after checking if it has already been calculated

cacheSolve <- function(x, ...) {
    
    inv_M <- x$getInverse()
    
    if(!is.null(inv_M)) {
        message("getting cached data")
        return(inv_M)
    }
    
    data <- x$get()
    inv_M <- solve(data, ...)
    x$setInverse(inv_M)
    
    inv_M
        
}



