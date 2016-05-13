## The function creates a matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function () x
                setinverse <- function(inverse) m <<- inverse
                getinverse <- function() m
                list (set = set, get = get,
                      setinverse = setinverse,
                      getinverse = getinverse)
}


## Calculates the inverse, or if it is cached it retrieves it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
