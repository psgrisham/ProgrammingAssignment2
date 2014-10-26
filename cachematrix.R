# Assignment #2 - Paul Grisham

makeCacheMatrix <- function(original = matrix()) {

    # The original matrix is preserved in this environment as "original"
    # Cache of the inverse matrix, initialize to un-calculated state
    invmat <- NULL
    set <- function(y) {
        # Assign the new matrix object to the cache of the original matrix
        original <<- y
        # Reset the inverse matrix to the un-calculated state
        invmat <<- NULL
    }
    get <- function() original
    setinv <- function(i) invmat <<- i
    getinv <- function() invmat
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    # First, check if the inverse has already been calculated
    invmat <- x$getinv()
    if (!is.null(invmat)) {
        # The cached inverse is ready to use
        message("getting cached data")
        # Return the cahced value
        return(invmat)
    }
    # The value was not cached. This is because getinv() has not been called yet
    # or because the value of the matrix has changed since the inverse was calculated.
    # Calculate the inverse matrix and cache it
    original <- x$get()
    # Calculate the inverse matrix
    invmat <- solve(original)
    # Cache the inverse matrix
    x$setinv(invmat)
    # Return the inverse matrix
    invmat
}
