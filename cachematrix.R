
## makeCacheMatrix()
## creates a matrix object that can cache its inverse
## Returns a list containing functions to
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse
##      4. get the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                ## use '<<-" to assign a value to an object 
                ## that is in a different environment
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve()
## Returns a matrix that is the inverse of 'x'.  
## If the inverse has already been calculated, 
## cacheSolve() pulls it from the cache and does 
## not compute the inverse again.
cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        ## if the inverse was already calculated, get the cached value
        if (!is.null(inv)) {
                message("getting cahced data")
                return(inv)
        }
        ## if the inverse wasn't already calculated, calculate it
        data <- x$get()
        inv <- solve(data, ...)
        ## set the cached inverse value using setinv()
        x$setinv(inv)
        inv
}
