
## General Description:
## This program has two functions: makeCacheMatrix and cacheSolve. 
## The first function is broadly used to create a storage structure for the Matrix and its Inverse. 
## The second function is overall used to calculate the Inverse of the Matrix, if 
## it is not already calculated (and is thus unavailable in the global environment). 
## If the inverse is already calculated, then if is fetched from the global environment.


## makeCacheMatrix Function Description:
## makeCacheMatrix creates a special vector of functions with four elements: set, get, setinv and getinv. 
## The above-mentioned four elements are structural definitions of four functions, used to set and get the Matrix and its Inverse respectively.
## Variable 'x' refers to the matrix. One of the ways to put values in it is using rbind/cbind functions to bind some vectors.
## Variable 's' is supposed to be the inverse of the Matrix, which is actually calculated in the cacheSolve function later.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL }
    
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve Function Description:
## cacheSolve searches for the presence of the Inverse of the Matrix, in the global environment.
## If the Inverse is present, it's value is returned.
## If the Inverse of the Matrix is not present, it is constructed by using the solve function.

cacheSolve <- function(x, ...) {
    s <- x$getinv()
    if(!is.null(s)) {
       message("Getting Cached Data")
       return(s) }
    
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}