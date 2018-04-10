#Assuming the matrix supplied is always invertible/adopting the solve function, #makeCacheMatrix creates a special "matrix" object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

#cacheSolve computes the inverse of the special matrix; if the inverse is #already calculated (for the same matrix), the reverse from the cache will be #retrieved.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
