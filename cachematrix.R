## These two functions allow the inverse of a matrix to be cached for ease of
## access and more efficient callbacks. 

## The function makeCacheMatrix provides tools to set and cache the inverse of
## a matrix.

makeCacheMatrix <- function(x = numeric()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getmean <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve finds the inverse of the matrix if it does not already exist, else
## the function reads the cached inverse. 

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    ## Return a matrix that is the inverse of 'x'
    i
}
