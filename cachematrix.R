## Pair of functions that cache the inverse of a matrix,
## Since matrix inversion is usually costly.
## The first function creates a special matrix and returns a list of 
## operations on it: get, set, getInverse, setInverse
## The second function calculates the inverse of the special "matrix" 
## created with the first function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation 

## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    # the solve function returns the inverse of a matrix
    inv <- solve(data, ...) 
    x$setInverse(inv)
    inv
}
