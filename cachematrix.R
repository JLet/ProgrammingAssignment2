## Here are two functions that create a special object that 
## stores a matrix and cache its inverse if it is a square invertible matrix. 


## makeCacheMatrix() is a function that creates a list of functions:
##      1.set: set the value of the matrix
##      2.get: get the value of the matrix
##      3.setinverse: set the inverse of the matrix
##      4.getinverse: get the inverse of the matrix, if it's already calculated.
## It stores an inputted matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() {x}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve is first checking whether the inverse of the inputted matrix is
## already calculated and putting the string "getting cached data" and the value out
## if there is a value in getinverse(). Else it takes the stored matrix out of get(),
## calculates its inverse and caches it in setinverse(). It returns the inverse of 
## the inputted matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
