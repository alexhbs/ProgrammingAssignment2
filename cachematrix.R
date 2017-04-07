## makeCacheMatrix() will receive matrix x and return a list of functions:
## get & set: get and set internal matrix value to be used by cacheSolve()
## getinverse & setinverse: gets or sets the received inverse of "x"

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() will test if the inverse of matrix "x" has been calculated and
## (it should be cached within object "x" in the scope of makeCacheMatrix() if so)
## if not, calculates the inverse of such matrix, returns it and then caches it
## for faster access

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

