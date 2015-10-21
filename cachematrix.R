## Programming Assignment #2: Caching the Inverse
## of a matrix.

## Function makeCacheMatrix creates a matrix object, then
## defines setsolve function to find inverse of matrix and
## cache the result. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
}
get <- function() x
setsolve <- function(solve) m <<- solve
getsolve <- function() m
list(set = set, get = get,
     setsolve = setsolve,
     getsolve = getsolve)
}

## Function cacheSolve determines whether the inverse 
## of the matrix created by makeCacheMatrix has
## already been computed, and if so, displays the cached
## result. If not, it computes the inverse for the matrix
## and displays the result.

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