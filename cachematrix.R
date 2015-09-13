## makeCacheMatrix accepts x, which by default is an empty matrix.
## cacheSolve, caches the inverse of a matrix.

## set and get self explanatory. attempt to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(set = set, get = get,
	setinv = setinv,
	getinv = getinv)
}

## get the matrix from makeCacheMatrix, and look for the inverse
## in cache. if found, retrieve it. if not found, then solve and 
## cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
