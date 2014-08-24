##
## The following two functions allows you to create a special object that stores
## a numeric matrix and cache's it's inverse matrix.  The inverse matrix will
## only be calculated if the inverse was not previously cached.
##

## Instantiate an object that stores a matrix and can cache its inverse matrix.
## It is assumed that a square invertible matrix is passed as input.
## 'x' stores the matrix
## 'cacheInverse' stores the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    cacheInverse <- NULL                # on object instantiation, 
                                        # . clear inverse matrix cache
    
    set <- function(y) {                # changing matrix of existing object
        if (!identical(x,y)) {          # to be efficient, clear cache of
            cacheInverse <<- NULL       # . inverse only if the matrix  
        }                               # . actually changed
        x <<- y                         # save new matrix    
    }
    get <- function() x                 # method to return original matrix
    setInverse <- function(aMatrix) {
        cacheInverse <<- aMatrix        # method to cache inverse matrix
    }
    getInverse <- function() {
        cacheInverse                    # method to return cached inverse
    }
    list(set = set, get = get,          # return a list to expose functions
         setInverse = setInverse,
         getInverse = getInverse)
}


## Return a matrix that is the inverse of a matrix saved in a special object
## created by the makeCacheMatrix() function.  This function returns the cached
## inverse matrix if available.  If not, solve() function is called to
## calculate the inverse and this value is returned and also saved in cache for
## next time this function is called.

cacheSolve <- function(x, ...) {
    anInverse <- x$getInverse()         # get cache value for inverse matrix
    if(!is.null(anInverse)) {           # check if cache matrix exists
        message("getting cached data")  # no need to re-calculate inverse
                                        # so just return cached value
    } else {
        data <- x$get()                 # get original matrix
        anInverse <- solve(data, ...)   # calculate the inverse of matrix
        x$setInverse(anInverse)         # cache inverse matrix in x object
    }
    anInverse                           # return inverse matrix
}
