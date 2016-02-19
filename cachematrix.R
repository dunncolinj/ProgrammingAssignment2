## Programming Assignment 2: Lexical Scoping
## Two functions defined, makeCacheMatrix and cacheSolve
##
## makeCacheMatrix takes a matrix as input. It creates an object
## with four methods, set, get, getinv, and setinv.
##
## set - sets the value of the caching matrix
## get - gets the value of the caching matrix
## getinv - gets the inverse of the matrix from cache; returns NULL if inverse not yet computed and stored
## setinv - sets the inverse of the matrix
##
## cacheSolve takes a caching matrix as input.
## It returns the inverse of the matrix.
## If the inverse has not been stored in the cache, it is stored for future retrieval.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    getinv <- function() inv
    setinv <- function(inverse) inv <<- inverse
    list(set = set, get = get, getinv = getinv, setinv = setinv)
}

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m) ## invert the matrix
    x$setinv(i) ## store the inverse in cache
    return(i) ## return the inverse
}
