## The two functions below are useful to avoid repeating the calculation of the inverse of a matrix of  
## interest. The inverse matrix is stored in the cache and retreived if necessary.

## The makeCacheMatrix function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
			m <- NULL
			set <- function(y) {
				x <<- y
				m <<- NULL
			}
			get <- function() x
			setmat <- function(solve) m <<- solve
			getmat <- function() m
			list(set = set, get = get, setmat = setmat, getmat = getmat)
}


## The cacheSolve function computes the inverse of the matrix created above and checks if the inverse has
## already been calculated. In that case the result is retreived from the cache.

cacheSolve <- function(x, ...) {
        	m <- x$getmat()
        	if(!is.null(m)) {
        			message("getting cached data")
        			return(m)
        	}
        	data <- x$get()
        	m <- solve(data, ...)
        	x$setmat(m)
        	m
}
