## This program includes two functions
##      1) A function that creates a matrix object that can cache its inverse
##      2) A function that computes and returns the inverse 
##         (from the cache if already computed)

## The makeCacheMatrix function creates a "matrix" object that can cache its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() { x }
    setinverse <- function(inverse) {inv <<- inverse}
    getinverse <- function() { inv }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The "cacheSolve" function returns the inverted matrix. The first time it's 
## called it stores the inverse in the cache using the makeCacheMatrix function
## Subsequently the inverse is returned from the cache without having to 
## recompute using the "solve" function if the matrix has not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- try(solve(data), silent = TRUE)
    if (class(inv) == "try-error") {
        return("Please supply an invertible matrix")
    }
    x$setinverse(inv)
    inv
}