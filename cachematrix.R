## This script has 2 functions, the first one creates a 
## special tipe of matrix (R Object) that stores a matrix and its inverse.
##The second one requires an object of the same type of the first one
## as argument and retrive the inverse function that is cached in memory
## and stored in the first one.


## creates a special tipe of matrix (R Object) that stores a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## requires an argument that is returned by makeCacheMatrix() in order 
##to retrieve the inverse matrix from the cached value that is stored in the 
##makeCacheMatrix() object's environment.
cacheSolve <- function(x, ...) {
        
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
   
    ## Return a matrix that is the inverse of 'x'
     s
}
