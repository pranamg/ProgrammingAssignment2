## Matrix inversion is usually a costly computation and their may be 
## some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly The two functions listed below cache the inverse of a matrix. 
 
## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        l <- NULL
        set <- function(y) {
                x <<- y
                l <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) l <<- solve
        getInverse <- function() l
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        l <- x$getInverse()
        
        if(!is.null(l)) {
                message("getting cached data")
                return(l)
        }
        
        data <- x$get()
        l <- solve(data, ...)
        x$setInverse(l)
        l
}