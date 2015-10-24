## Put comments here that give an overall description of what your
## functions do

## Returns a Matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("Cached data coming right up!")
        return(m)
    }
    message("Oh no! We have no cache. We'll need to calculate it ourselves.")
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    return(m)
}