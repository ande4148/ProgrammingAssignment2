## The function makeCacheMatrix loads a designated matrix, calculates it's inverse, and
## stores the inverse in the parent environment from which the matrix was called

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The function cachesolve returns the inverse of an inverted matrix.  The inverted matrix is either
## drawn from the parent environment if the original matrix has already been inverted, or, 
## it inverts the original matrix so that it can have its inverse calculated and returned.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
