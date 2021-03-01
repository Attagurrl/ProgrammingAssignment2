## This code solves for the inverse of a square matrix and caches the result. This could come in handy by saving a lot of computational time incase of large datasets.
## The "makeCasheMatrix" function gets/sets the matrix and its inverse.
## Incase the inverse has already been calculated (and the matrix has not changed), then the "cacheSolve" function retrieves the inverse from the first function.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
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
