## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize cached inverse as NULL
    set <- function(y) {
        x <<- y  # Assign new matrix
        inv <<- NULL  # Reset cached inverse
    }
    get <- function() x  # Return the current matrix
    setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
    getInverse <- function() inv  # Retrieve cached inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()  # Check for cached inverse
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)  # Return cached inverse if available
        }
        data <- x$get()  # Get the current matrix
        inv <- solve(data, ...)  # Compute inverse
        x$setInverse(inv)  # Cache the computed inverse
        inv
}
